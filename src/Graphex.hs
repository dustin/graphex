{-# LANGUAGE OverloadedRecordDot #-}
module Graphex (Graph(..),
    reverseEdges, directDepsOn, allDepsOn,
    why, rankings, allPathsTo, restrictTo, mapMaybeWithKey,
    graphToDep, depToGraph, graphToTree) where

import           Control.Monad               (ap)
import           Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import           Data.Map                    (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe, mapMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           Data.Tree                   (Tree)
import qualified Data.Tree                   as Tree

import           Graphex.Core                (Graph (..))
import           Graphex.LookingGlass
import           Graphex.Search              (bfsOn, findFirst, flood)

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
allDepsOn = flood . directDepsOn

-- | Find an example path between two modules.
--
-- This is a short path, but the important part is that it represents how connectivy works.
why :: Graph -> Text -> Text -> [Text]
why m from to = fromMaybe [] $ findFirst bfsOn head (\ks -> (:ks) <$> (Set.toList . directDepsOn m . head) ks) [from] ((== to) . head)

-- | Find all paths between two modules as a restricted graph of the intersection of
-- reachable nodes from the start and to the end.
allPathsTo :: Graph -> Text -> Text -> Graph
allPathsTo m from to = restrictTo m $ allDepsOn m from `Set.intersection` allDepsOn (reverseEdges m) to

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

-- | Convert a Graph to a Tree. If there is a cycle, treat the cycle point as a leaf.
graphToTree :: Text -> Graph -> Tree Text
graphToTree startingAt (Graph rels) = go mempty startingAt
  where
    go seen node =
      let children = Set.toList $ Map.findWithDefault mempty node rels
      in Tree.Node node $ if (Set.member node seen) then [] else fmap (go (Set.insert node seen)) children
