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
