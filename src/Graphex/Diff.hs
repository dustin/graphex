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

data Diff a = Diff
  { netNodes         :: ~Int
  , nodes            :: ~(Map a Int)

  , netReversedNodes :: ~Int
  , reversedNodes    :: ~(Map a Int)
  } deriving stock (Show)

diff :: NFData a => Ord a => Graph a -> Graph a -> Diff a
diff g1 g2 = Diff{..}
  where
    g1rev = reverseEdges g1
    g2rev = reverseEdges g2

    r1 = rankings g1
    r2 = rankings g2

    r1rev = rankings g1rev
    r2rev = rankings g2rev

    doDiff x y =
      let x' = Map.fromList $ fmap (second (Sum . negate) . swap) x
          y' = Map.fromList $ fmap (second Sum . swap) y
      in Map.filter (/= 0) $ Map.map getSum $ salign x' y'

    nodes = doDiff r1 r2
    netNodes = sum nodes

    reversedNodes = doDiff r1rev r2rev
    netReversedNodes = sum reversedNodes
