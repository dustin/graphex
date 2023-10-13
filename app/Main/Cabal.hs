{-# LANGUAGE ApplicativeDo #-}
module Main.Cabal where

import           Data.Aeson           (encode)
import           Data.Bool            (bool)
import qualified Data.ByteString.Lazy as BL
import           Data.List.NonEmpty   (NonEmpty (..), nonEmpty)
import Data.Maybe (fromMaybe)
import           Options.Applicative

import           Graphex.Cabal
import           Graphex.LookingGlass

data CabalOptions = CabalOptions
  { optDiscoverExes    :: Bool
  , optDiscoverTests   :: Bool
  , optIncludeExternal :: Bool
  } deriving stock Show

-- TODO: Do a left-to-right-preserving discover opts parser
cabalOptions :: Parser CabalOptions
cabalOptions = do
  optDiscoverExes <- switch (long "discover-exes" <> help "Discover exe import dependencies")
  optDiscoverTests <- switch (long "discover-tests" <> help "Discover test import dependencies")
  optIncludeExternal <- switch (long "include-external" <> help "Include external import dependencies")
  pure CabalOptions{..}

data CabalOptions2 = CabalOptions2
  { optToDiscover :: [CabalDiscoverType]
  , optIncludeExternal2 :: Bool
  } deriving stock Show

justWhen :: a -> Bool -> Maybe a
justWhen a True = Just a
justWhen _ False = Nothing

cabalOptions2 :: Parser CabalOptions2
cabalOptions2 = do
  optToDiscover <- fmap mconcat $ many $ asum
    [ flag' [CabalDiscoverAll CabalLibrary, CabalDiscoverAll CabalExecutable, CabalDiscoverAll CabalTests] (long "discover-all" <> help "Discover all import dependencies")
    , flag' (pure $ CabalDiscoverAll CabalLibrary) (long "discover-all-libs" <> help "Discover all library import dependencies")
    , flag' (pure $ CabalDiscoverAll CabalExecutable) (long "discover-all-exes" <> help "Discover all executable import dependencies")
    , flag' (pure $ CabalDiscoverAll CabalTests) (long "discover-all-tests" <> help "Discover all test import dependencies")
    , flag' (pure $ CabalDontDiscoverAll CabalLibrary) (long "no-discover-all-libs" <> help "Discover no library import dependencies")
    , flag' (pure $ CabalDontDiscoverAll CabalExecutable) (long "no-discover-all-exes" <> help "Discover no executable import dependencies")
    , flag' (pure $ CabalDontDiscoverAll CabalTests) (long "no-discover-all-exes" <> help "Discover no test import dependencies")
    , flag' (pure $ CabalDiscover (CabalLibraryUnit Nothing)) (long "discover-lib" <> help "Discover the default library import dependencies")
    , flag' (pure $ CabalDontDiscover (CabalLibraryUnit Nothing)) (long "no-discover-lib" <> help "Don't discover the default library import dependencies")
    , pure . CabalDiscover . CabalLibraryUnit . Just <$> strOption (long "discover-sublib" <> help "Discover specified sublibrary import dependencies")
    , pure . CabalDontDiscover . CabalLibraryUnit . Just <$> strOption (long "no-discover-sublib" <> help "Don't discover specified sublibrary import dependencies")
    , pure . CabalDiscover . CabalExecutableUnit <$> strOption (long "discover-exe" <> help "Discover specified executable import dependencies")
    , pure . CabalDontDiscover . CabalExecutableUnit <$> strOption (long "no-discover-exe" <> help "Don't discover specified executable import dependencies")
    , pure . CabalDiscover . CabalTestsUnit <$> strOption (long "discover-test" <> help "Discover specified test import dependencies")
    , pure . CabalDontDiscover . CabalTestsUnit <$> strOption (long "no-discover-test" <> help "Don't discover specified test import dependencies")
    ]
  optIncludeExternal2 <- switch (long "include-external" <> help "Include external import dependencies")
  pure CabalOptions2{..}

runCabal2 :: CabalOptions2 -> IO ()
runCabal2 CabalOptions2{..} = do
  let discoverOpts = CabalDiscoverOpts
        { toDiscover = fromMaybe (pure $ CabalDiscover (CabalLibraryUnit Nothing)) $ nonEmpty optToDiscover
        , includeExternal = optIncludeExternal2
        }
  mg <- discoverCabalModuleGraph discoverOpts
  BL.putStr $ encode $ toLookingGlass "Internal Package Dependencies" mempty mg

runCabal :: CabalOptions -> IO ()
runCabal CabalOptions{..} = do
  let discoverOpts = CabalDiscoverOpts
        { toDiscover = CabalDiscoverAll CabalLibrary :| mconcat
          [ bool mempty [CabalDiscoverAll CabalExecutable] optDiscoverExes
          , bool mempty [CabalDiscoverAll CabalTests] optDiscoverTests
          ]
        , includeExternal = optIncludeExternal
        }
  mg <- discoverCabalModuleGraph discoverOpts
  BL.putStr $ encode $ toLookingGlass "Internal Package Dependencies" mempty mg
