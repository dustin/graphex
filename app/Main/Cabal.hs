{-# LANGUAGE ApplicativeDo #-}
module Main.Cabal where

import           Data.Aeson           (encode)
import           Data.Bool            (bool)
import qualified Data.ByteString.Lazy as BL
import           Data.List.NonEmpty   (NonEmpty (..))
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
  optToDiscover <- many $ asum
    [ flag' (CabalDiscoverAll CabalLibrary) (long "discover-all-libs" <> help "Discover all library import dependencies")
    , flag' (CabalDiscoverAll CabalExecutable) (long "discover-all-exes" <> help "Discover all executable import dependencies")
    , flag' (CabalDiscoverAll CabalTests) (long "discover-all-tests" <> help "Discover all test import dependencies")
    , flag' (CabalDontDiscoverAll CabalLibrary) (long "no-discover-all-libs" <> help "Discover no library import dependencies")
    , flag' (CabalDontDiscoverAll CabalExecutable) (long "no-discover-all-exes" <> help "Discover no executable import dependencies")
    , flag' (CabalDontDiscoverAll CabalTests) (long "no-discover-all-exes" <> help "Discover no test import dependencies")
    -- TODO: Do the same but for granular (use strOption)
    ]
  optIncludeExternal2 <- switch (long "include-external" <> help "Include external import dependencies")
  pure CabalOptions2{..}

runCabal2 :: CabalOptions2 -> IO ()
runCabal2 = print

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
