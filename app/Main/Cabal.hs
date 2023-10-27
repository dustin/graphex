{-# LANGUAGE ApplicativeDo #-}
module Main.Cabal where

import           Control.Concurrent   (getNumCapabilities)
import           Data.Aeson           (encode)
import qualified Data.ByteString.Lazy as BL
import           Data.List.NonEmpty   (nonEmpty)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import           Options.Applicative
import           Text.Regex.TDFA

import           Graphex.Cabal
import           Graphex.Core
import           Graphex.Logger
import           Graphex.LookingGlass

data CabalOptions = CabalOptions
  { optToDiscover      :: [CabalDiscoverType]
  , optIncludeExternal :: Bool
  , optNumJobs         :: Maybe Int
  , optPruneTo         :: [ModuleName]
  , optPruneToRegex    :: [Text]
  } deriving stock Show

justWhen :: a -> Bool -> Maybe a
justWhen a True  = Just a
justWhen _ False = Nothing

cabalOptions :: Parser CabalOptions
cabalOptions = do
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
  optIncludeExternal <- switch (long "include-external" <> help "Include external import dependencies")
  optNumJobs <- optional (option auto (long "jobs" <> short 'j' <> help "Number of worker threads to use"))
  optPruneTo <- many $ strOption (long "prune-to" <> help "Only discover import dependencies of the specified module(s)")
  optPruneToRegex <- many $ strOption (long "prune-to-regex" <> help "Only discover import dependencies of modules that match a regex")
  pure CabalOptions{..}

runCabal :: CabalOptions -> IO ()
runCabal CabalOptions{..} = do
  numCapabilities <- getNumCapabilities
  let numJobs =fromMaybe numCapabilities optNumJobs
  logit $ unwords ["Discovering with num jobs = ", show numJobs]

  let pruneToExplicit = flip elem <$> nonEmpty optPruneTo
  let discoverOpts = CabalDiscoverOpts
        { toDiscover = fromMaybe (pure $ CabalDiscover (CabalLibraryUnit Nothing)) $ nonEmpty optToDiscover
        , includeExternal = optIncludeExternal
        , pruneTo = pruneToExplicit
        , ..
        }
  mg <- discoverCabalModuleGraph discoverOpts
  BL.putStr $ encode $ toLookingGlass "Internal Package Dependencies" mempty mg
