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
  ]


mkDiscoverCabalModulesUnit :: [Module] -> CabalDiscoverOpts -> IO ()
mkDiscoverCabalModulesUnit (sort -> mods) opts = assertEqual "" mods . sort =<< discoverCabalModules opts "graphex.cabal"

unit_libCabalModules :: IO ()
unit_libCabalModules = mkDiscoverCabalModulesUnit libModules CabalDiscoverOpts
  { toDiscover = Set.singleton CabalLibraries
  , includeExternal = False
  }

unit_exeCabalModules :: IO ()
unit_exeCabalModules = mkDiscoverCabalModulesUnit exeModules CabalDiscoverOpts
  { toDiscover = Set.singleton CabalExecutables
  , includeExternal = False
  }

unit_testCabalModules :: IO ()
unit_testCabalModules = mkDiscoverCabalModulesUnit testModules CabalDiscoverOpts
  { toDiscover = Set.singleton CabalTests
  , includeExternal = False
  }

unit_discoverModules :: IO ()
unit_discoverModules = do
    g <- discoverCabalModuleGraph CabalDiscoverOpts{toDiscover = Set.singleton CabalLibraries, includeExternal = False}
    assertBool (show g) . isJust $ why g "Graphex.Parser" "Graphex.Core"