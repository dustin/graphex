{-# LANGUAGE FlexibleInstances #-}
module HpackSpec where

import           Data.List        (sort)
import           Data.Maybe       (isJust)
import           Test.Tasty.HUnit

import           Graphex
import           Graphex.Cabal
import           Graphex.Core
import           Graphex.Hpack

import           TestInstances    ()

defaultOpts :: HpackDiscoverOpts
defaultOpts = HpackDiscoverOpts
  { hpackToDiscover = pure $ CabalDiscoverAll CabalLibrary
  , hpackIncludeExternal = False
  , hpackNumJobs = 4
  , hpackPruneTo = Nothing
  }

mkDiscoverHpackModulesUnit :: [Module] -> HpackDiscoverOpts -> IO ()
mkDiscoverHpackModulesUnit (sort -> mods) opts = assertEqual "" mods . sort =<< discoverHpackModules opts "package.yaml"

-- The library modules should match what Cabal discovers
unit_hpackLibModules :: IO ()
unit_hpackLibModules = mkDiscoverHpackModulesUnit (sort $ dummySublibModules ++ libModules) defaultOpts
  { hpackToDiscover = pure $ CabalDiscoverAll CabalLibrary
  }

unit_hpackExeModules :: IO ()
unit_hpackExeModules = mkDiscoverHpackModulesUnit exeModules defaultOpts
  { hpackToDiscover = pure $ CabalDiscoverAll CabalExecutable
  }

unit_hpackTestModules :: IO ()
unit_hpackTestModules = mkDiscoverHpackModulesUnit testModules defaultOpts
  { hpackToDiscover = pure $ CabalDiscoverAll CabalTests
  }

unit_hpackSublibModules :: IO ()
unit_hpackSublibModules = mkDiscoverHpackModulesUnit dummySublibModules defaultOpts
  { hpackToDiscover = pure $ CabalDiscover (CabalLibraryUnit (Just "graphex-dummy-sublib"))
  }

unit_hpackDiscoverGraph :: IO ()
unit_hpackDiscoverGraph = do
    CabalGraph{..} <- discoverHpackModuleGraph defaultOpts
      { hpackToDiscover = pure $ CabalDiscoverAll CabalLibrary
      , hpackIncludeExternal = False
      } "package.yaml"
    assertBool (show moduleGraph) . isJust $ why moduleGraph "Graphex.Parser" "Graphex.Core"

unit_pathToModuleName :: IO ()
unit_pathToModuleName = do
    assertEqual "simple" "Graphex.Core" $ pathToModuleName "src" "src/Graphex/Core.hs"
    assertEqual "top-level" "Main" $ pathToModuleName "app" "app/Main.hs"
    assertEqual "nested" "Graphex.Cabal" $ pathToModuleName "src" "src/Graphex/Cabal.hs"

-- Module lists matching CabalSpec, but graphex's package.yaml doesn't have
-- explicit exposed-modules, so hpack will glob. These lists represent what
-- globbing should find, which should match what Cabal discovers.
libModules :: [Module]
libModules =
  [ Module "Graphex" "src/Graphex.hs"
  , Module "Graphex.Cabal" "src/Graphex/Cabal.hs"
  , Module "Graphex.Core" "src/Graphex/Core.hs"
  , Module "Graphex.CSV" "src/Graphex/CSV.hs"
  , Module "Graphex.Hpack" "src/Graphex/Hpack.hs"
  , Module "Graphex.LookingGlass" "src/Graphex/LookingGlass.hs"
  , Module "Graphex.Parser" "src/Graphex/Parser.hs"
  , Module "Graphex.Search" "src/Graphex/Search.hs"
  , Module "Graphex.Logger" "src/Graphex/Logger.hs"
  , Module "Graphex.Queue" "src/Graphex/Queue.hs"
  , Module "Graphex.Diff" "src/Graphex/Diff.hs"
  ]

dummySublibModules :: [Module]
dummySublibModules =
  [ Module "DummySublibModule" "dummy-sublib/DummySublibModule.hs"
  , Module "DummyTest" "dummy-sublib/test/DummyTest.hs"
  ]

-- Exe: hpack globs "app" dir, so it discovers all .hs files there
exeModules :: [Module]
exeModules =
  [ Module "graphex-Main" "app/Main.hs"
  , Module "Main.Cabal" "app/Main/Cabal.hs"
  , Module "Main.Hpack" "app/Main/Hpack.hs"
  ]

-- Tests: hpack globs "test" dir
testModules :: [Module]
testModules =
  [ Module "CabalSpec" "test/CabalSpec.hs"
  , Module "HpackSpec" "test/HpackSpec.hs"
  , Module "ImportParserSpec" "test/ImportParserSpec.hs"
  , Module "Spec" "test/Spec.hs"
  , Module "TestInstances" "test/TestInstances.hs"
  ]
