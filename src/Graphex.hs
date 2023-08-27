{-# LANGUAGE OverloadedRecordDot #-}
module Graphex (Graph(..),
    reverseEdges, directDepsOn, allDepsOn, allDepsOnWithKey,
    why, rankings, allPathsTo, restrictTo, mapMaybeWithKey,
    graphToDep, depToGraph) where

import           Algorithm.Search            (dijkstra)
import           Control.Monad               (ap)
import           Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import           Data.Map                    (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (mapMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)

import           Graphex.Core                (Graph (..))
import           Graphex.LookingGlass

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

-- | Flood fill to find all transitive dependencies on a starting module, excluding the original key.
allDepsOn :: Graph -> Text -> Set Text
allDepsOn m k = Set.delete k (allDepsOnWithKey m k)

-- | Flood fill to find all transitive dependencies on a starting module, including the original key.
allDepsOnWithKey :: Graph -> Text -> Set Text
allDepsOnWithKey m = go mempty . Set.singleton
    where
        go !s (flip Set.difference s -> todo)
            | Set.null todo = s
            | otherwise = go (s <> todo) (Set.unions $ Set.map (directDepsOn m) todo)

-- | Find an example path between two modules.
--
-- This is a short path, but the important part is that it represents how connectivy works.
why :: Graph -> Text -> Text -> [Text]
why m from to = maybe [] snd $ dijkstra (directDepsOn m) (const (const (1::Int))) (== to) from

-- | Find all paths between two modules as a restricted graph of the intersection of
-- reachable nodes from the start and to the end.
allPathsTo :: Graph -> Text -> Text -> Graph
allPathsTo m from to = restrictTo m $ allDepsOnWithKey m from `Set.intersection` allDepsOnWithKey (reverseEdges m) to

-- | Count the number of transitive dependencies for each module.
rankings :: Graph -> Map Text Int
rankings g = Map.fromList $ mapMaybeWithKey (Just . length . allDepsOn g) g

-- | Visit each node in the graph and apply a function to it.
mapMaybeWithKey :: NFData a => (Text -> Maybe a) -> Graph -> [(Text, a)]
mapMaybeWithKey f = mapMaybe sequenceA . parMap rdeepseq (ap (,) f) . Map.keys . unGraph

-- | Restrict a graph to only the given set of modules.
restrictTo :: Graph -> Set Text -> Graph
restrictTo (Graph m) keep = Graph . flip Map.mapMaybeWithKey m $ \k' v ->
  (if Set.notMember k' keep then Nothing else nonNullSet (Set.intersection keep v))
    where
        nonNullSet v = if Set.null v then Nothing else Just v
