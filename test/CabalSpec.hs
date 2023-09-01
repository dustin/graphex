{-# LANGUAGE FlexibleInstances #-}
module CabalSpec where

import           Data.Foldable    (fold)
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (isJust)
import qualified Data.Set         as Set
import           Test.Tasty
import           Test.Tasty.HUnit

import           Graphex
import           Graphex.Cabal
import           Graphex.Core

myModules :: [Module]
myModules = [Module "Graphex" "src/Graphex.hs",
             Module "Graphex.Cabal" "src/Graphex/Cabal.hs",
             Module "Graphex.Core" "src/Graphex/Core.hs",
             Module "Graphex.CSV" "src/Graphex/CSV.hs",
             Module "Graphex.LookingGlass" "src/Graphex/LookingGlass.hs",
             Module "Graphex.Parser" "src/Graphex/Parser.hs",
             Module "Graphex.Search" "src/Graphex/Search.hs"]

unit_myCabalModules :: IO ()
unit_myCabalModules = assertEqual "" myModules =<< discoverCabalModules "graphex.cabal"

unit_discoverModules :: IO ()
unit_discoverModules = do
    g <- discoverCabalModuleGraph
    assertBool (show g) . isJust $ why g "Graphex.Parser" "Graphex.Core"
