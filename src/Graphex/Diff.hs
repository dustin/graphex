{-# LANGUAGE StrictData #-}
module Graphex.Diff where

import           Control.Arrow               (second)
import           Control.Parallel.Strategies (NFData)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Monoid                 (Sum (..))
import           Data.Semialign
import           Data.Tuple                  (swap)
import           Graphex
import           Graphex.Core

data Diff a = Diff
  { nodes         :: ~(Map a Int)
  , reversedNodes :: ~(Map a Int)
  , edges         :: ~(Map a Int)
  , reversedEdges :: ~(Map a Int)
  }

diff :: NFData a => Ord a => Graph a -> Graph a -> Diff a
diff g1 g2 = Diff{..}
  where
    g1rev = reverseEdges g1
    g2rev = reverseEdges g2
    ranks1 = Map.fromList $ fmap (second (Sum . negate) . swap) $ rankings g1
    ranks2 = Map.fromList $ fmap (second Sum . swap) $ rankings g2
    nodes = Map.filter (/= 0) $ Map.map getSum $ salign ranks1 ranks2
    ranks1rev = Map.fromList $ fmap (second (Sum . negate) . swap) $ rankings g1rev
    ranks2rev = Map.fromList $ fmap (second Sum . swap) $ rankings g2rev
    reversedNodes = Map.filter (/= 0) $ Map.map getSum $ salign ranks1rev ranks2rev
    edges = mempty
    reversedEdges = mempty


