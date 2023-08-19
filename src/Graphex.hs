{-# LANGUAGE OverloadedRecordDot #-}
module Graphex (Graph(..), reverseEdges, directDepsOn, allDepsOn, why, rankings, restrictTo, graphToDep, depToGraph) where

import           Algorithm.Search            (dijkstra)
import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.Map                    (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)

import Graphex.Core (Graph (..))
import Graphex.LookingGlass

-- | Convert a dependency file to a graph.
depToGraph :: GraphDef -> Graph
depToGraph GraphDef{..} = Graph (links <> allNodes)
    where
        links = Map.fromListWith (<>) [ (name from, Set.singleton (name to)) | Edge{..} <- edges]
        allNodes = Map.fromList [(label, mempty) | Node{..} <- Map.elems nodes]
        name = (.label) . (nodes Map.!) :: Text -> Text

-- | Convert a graph back to our dependency file format.
graphToDep :: Graph -> GraphDef
graphToDep (Graph m) = GraphDef {
    title = "Internal Package Dependencies",
    edges = [ Edge { from = k, to = v } | (k, vs) <- Map.assocs m, v <- Set.toList vs ],
    nodes = Map.fromList [ (k, Node k Nothing) | k <- Map.keys m ]
    }

-- | Reverse all the arrows in the graphs.
reverseEdges :: Graph -> Graph
reverseEdges (Graph m) = Graph $ Map.fromListWith (<>) [ (v, Set.singleton k) | (k, vs) <- Map.assocs m, v <- Set.toList vs ] <> Map.fromSet (const mempty) (Map.keysSet m)

-- | Find the direct list of things that are referencing this module.
directDepsOn :: Graph -> Text -> Set Text
directDepsOn = flip (Map.findWithDefault mempty) . unGraph

-- Flood fill to find all transitive dependencies on a starting module.
allDepsOn :: Graph -> Text -> Set Text
allDepsOn m k = Set.delete k . go mempty . Set.singleton $ k
    where
        go !s (flip Set.difference s -> todo)
            | Set.null todo = s
            | otherwise = go (s <> todo) (Set.unions $ Set.map (directDepsOn m) todo)

-- | Find an example path between two modules.
--
-- This is a short path, but the important part is that it represents how connectivy works.
why :: Graph -> Text -> Text -> [Text]
why m from to = maybe [] snd $ dijkstra (directDepsOn m) (const (const (1::Int))) (== to) from

-- | Count the number of transitive dependencies for each module.
rankings :: Graph -> Map Text Int
rankings g@(Graph m) = Map.fromList $ parMap rdeepseq (\k -> (k, length $ allDepsOn g k)) (Map.keys m)

-- | Restrict a graph to only the modules that reference a given module.
restrictTo :: Graph -> Text -> Graph
restrictTo g@(Graph m) k = Graph . flip Map.mapMaybeWithKey m $ \k' v -> if k' == k then Just v else nonNullSet (Set.intersection keep v)
    where
        keep = allDepsOn g k
        nonNullSet v = if Set.null v then Nothing else Just v
