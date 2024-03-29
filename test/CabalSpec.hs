{-# LANGUAGE FlexibleInstances #-}
module CabalSpec where

import           Data.Foldable    (fold)
import           Data.List        (sort)
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (isJust)
import qualified Data.Set         as Set
import           Test.Tasty
import           Test.Tasty.HUnit

import           Graphex
import           Graphex.Cabal
import           Graphex.Core

import           TestInstances    ()

libModules :: [Module]
libModules =
  [ Module "Graphex" "src/Graphex.hs"
  , Module "Graphex.Cabal" "src/Graphex/Cabal.hs"
  , Module "Graphex.Core" "src/Graphex/Core.hs"
  , Module "Graphex.CSV" "src/Graphex/CSV.hs"
  , Module "Graphex.LookingGlass" "src/Graphex/LookingGlass.hs"
  , Module "Graphex.Parser" "src/Graphex/Parser.hs"
  , Module "Graphex.Search" "src/Graphex/Search.hs"
  , Module "Graphex.Logger" "src/Graphex/Logger.hs"
  , Module "Graphex.Queue" "src/Graphex/Queue.hs"
  , Module "Graphex.Diff" "src/Graphex/Diff.hs"
  ]

searchModuleGraph :: ModuleGraph
searchModuleGraph = mconcat
  [ mkGraph "Graphex.Search" ["Graphex.Queue"]
  , mkGraph "Graphex.Queue" []
  ]

testModules :: [Module]
testModules =
  [ Module "CabalSpec" "test/CabalSpec.hs"
  , Module "ImportParserSpec" "test/ImportParserSpec.hs"
  , Module "Paths_graphex" ModuleNoFile
  , Module "Spec"  "test/Spec.hs"
  , Module "TestInstances"  "test/TestInstances.hs"
  ]

exeModules :: [Module]
exeModules =
  [ Module "Paths_graphex" ModuleNoFile
  , Module "graphex-Main" "app/Main.hs"
  , Module "Main.Cabal" "app/Main/Cabal.hs"
  ]

dummySublibModules :: [Module]
dummySublibModules =
  [ Module "DummySublibModule" "dummy-sublib/DummySublibModule.hs"
  , Module "DummyTest" "dummy-sublib/test/DummyTest.hs"
  ]

defaultOpts :: CabalDiscoverOpts
defaultOpts = CabalDiscoverOpts
  { toDiscover = pure $ CabalDiscoverAll CabalLibrary
  , includeExternal = False
  , numJobs = 4
  , pruneTo = Nothing
  }

mkDiscoverCabalModulesUnit :: [Module] -> CabalDiscoverOpts -> IO ()
mkDiscoverCabalModulesUnit (sort -> mods) opts = assertEqual "" mods . sort =<< discoverCabalModules opts "graphex.cabal"

unit_libCabalModules :: IO ()
unit_libCabalModules = mkDiscoverCabalModulesUnit (dummySublibModules ++ libModules) defaultOpts
  { toDiscover = pure $ CabalDiscoverAll CabalLibrary
  , includeExternal = False
  }

unit_exeCabalModules :: IO ()
unit_exeCabalModules = mkDiscoverCabalModulesUnit exeModules defaultOpts
  { toDiscover = pure $ CabalDiscoverAll CabalExecutable
  , includeExternal = False
  }

unit_testCabalModules :: IO ()
unit_testCabalModules = mkDiscoverCabalModulesUnit testModules defaultOpts
  { toDiscover = pure $ CabalDiscoverAll CabalTests
  , includeExternal = False
  }

unit_sublibCabalModules :: IO ()
unit_sublibCabalModules = mkDiscoverCabalModulesUnit dummySublibModules defaultOpts
  { toDiscover = pure $ CabalDiscover (CabalLibraryUnit (Just "graphex-dummy-sublib"))
  , includeExternal = False
  }

unit_defaultLibCabalModules :: IO ()
unit_defaultLibCabalModules = mkDiscoverCabalModulesUnit libModules defaultOpts
  { toDiscover = pure $ CabalDiscover (CabalLibraryUnit Nothing)
  , includeExternal = False
  }

unit_granularExeCabalModules :: IO ()
unit_granularExeCabalModules = mkDiscoverCabalModulesUnit exeModules defaultOpts
  { toDiscover = pure $ CabalDiscover (CabalExecutableUnit "graphex")
  , includeExternal = False
  }

unit_granularTestCabalModules :: IO ()
unit_granularTestCabalModules = mkDiscoverCabalModulesUnit testModules defaultOpts
  { toDiscover = pure $ CabalDiscover (CabalTestsUnit "graphex-test")
  , includeExternal = False
  }

unit_discoverModules :: IO ()
unit_discoverModules = do
    CabalGraph{..} <- discoverCabalModuleGraph defaultOpts{toDiscover = pure $ CabalDiscoverAll CabalLibrary, includeExternal = False}
    assertBool (show moduleGraph) . isJust $ why moduleGraph "Graphex.Parser" "Graphex.Core"

unit_pruneToSearch :: IO ()
unit_pruneToSearch = do
  CabalGraph{..} <- discoverCabalModuleGraph defaultOpts
       { toDiscover = pure $ CabalDiscoverAll CabalLibrary
       , includeExternal = False
       , pruneTo = Just (== "Graphex.Search")
       }
  assertEqual "" moduleGraph searchModuleGraph
