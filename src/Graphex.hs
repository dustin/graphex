{-# LANGUAGE OverloadedRecordDot #-}
module Graphex (
    -- * The Graph
    Graph(..),
    -- * Working from an individual node in the graph.
    reverseEdges, directDepsOn, allDepsOn, why,
    -- * Working on the graph as a whole.
    rankings, longest, allPathsTo, restrictTo, mapMaybeWithKey,
    graphToDep, depToGraph, graphToTree) where

import           Control.Monad               (ap)
import           Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import           Data.Foldable               (maximumBy)
import           Data.Map                    (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe, listToMaybe, mapMaybe)
import           Data.Ord                    (comparing)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           Data.Tree                   (Tree)
import qualified Data.Tree                   as Tree

import           Graphex.Core                (Graph (..))
import           Graphex.LookingGlass
import           Graphex.Search              (bfsOn, dfsWith, findFirst, flood)

-- | Convert a dependency file to a graph.
depToGraph :: GraphDef -> Graph Text
depToGraph GraphDef{..} = Graph (links <> allNodes)
    where
        links = Map.fromListWith (<>) [ (name from, Set.singleton (name to)) | Edge{..} <- edges]
        allNodes = Map.fromList [(label, mempty) | Node{..} <- Map.elems nodes]
        name = (.label) . (nodes Map.!) :: Text -> Text

-- | Convert a graph back to our dependency file format.
graphToDep :: Graph Text -> GraphDef
graphToDep (Graph m) = GraphDef {
    title = "Internal Package Dependencies",
    edges = [ Edge { from = k, to = v } | (k, vs) <- Map.assocs m, v <- Set.toList vs ],
    nodes = Map.fromList [ (k, Node k Nothing) | k <- Map.keys m ]
    }

-- | Reverse all the arrows in the graphs.
reverseEdges :: Ord a => Graph a -> Graph a
reverseEdges (Graph m) = Graph $ Map.fromListWith (<>) [ (v, Set.singleton k) | (k, vs) <- Map.assocs m, v <- Set.toList vs ] <> Map.fromSet (const mempty) (Map.keysSet m)

-- | Find the direct list of things that are referencing this module.
directDepsOn :: Ord a => Graph a -> a -> Set a
directDepsOn = flip (Map.findWithDefault mempty) . unGraph

-- | Flood fill to find all transitive dependencies on a starting module, excluding the original key.
allDepsOn :: Ord a => Graph a -> a -> Set a
allDepsOn = flood . directDepsOn

-- | Find an example path between two modules.
--
-- This is a short path, but the important part is that it represents how connectivy works.
why :: Ord a => Graph a -> a -> a -> [a]
why m from to = fromMaybe [] $ findFirst bfsOn head (\ks -> (:ks) <$> (Set.toList . directDepsOn m . head) ks) [from] ((== to) . head)

-- | Find all paths between two modules as a restricted graph of the intersection of
-- reachable nodes from the start and to the end.
allPathsTo :: Ord a => Graph a -> a -> a -> Graph a
allPathsTo m from to = restrictTo m $ allDepsOn m from `Set.intersection` allDepsOn (reverseEdges m) to

-- | Count the number of transitive dependencies for each module.
rankings :: (NFData a, Ord a) => Graph a -> Map a Int
rankings g = Map.fromList $ mapMaybeWithKey (Just . length . allDepsOn g) g

-- | Visit each node in the graph and apply a function to it.
mapMaybeWithKey :: (NFData a, NFData g) => (g -> Maybe a) -> Graph g -> [(g, a)]
mapMaybeWithKey f = mapMaybe sequenceA . parMap rdeepseq (ap (,) f) . Map.keys . unGraph

-- | Restrict a graph to only the given set of modules.
restrictTo :: Ord a => Graph a -> Set a -> Graph a
restrictTo (Graph m) keep = Graph . flip Map.mapMaybeWithKey m $ \k' v ->
  (if Set.notMember k' keep then Nothing else nonNullSet (Set.intersection keep v))
    where
        nonNullSet v = if Set.null v then Nothing else Just v

-- | Convert a Graph to a Tree. If there is a cycle, treat the cycle point as a leaf.
graphToTree :: Ord a => a -> Graph a -> Tree a
graphToTree startingAt (Graph rels) = go mempty startingAt
  where
    go seen node =
      let children = Set.toList $ Map.findWithDefault mempty node rels
      in Tree.Node node $ if Set.member node seen then [] else fmap (go (Set.insert node seen)) children

-- | Find the longest path in a Graph.
longest :: (NFData a, Ord a) => Graph a -> [a]
longest g = reverse . snd . maximumOn (length . snd) $ mapMaybeWithKey one g
  where
    one = Just . fmap fst . maximumOn length . dfsWith store seen (\ks -> (:ks) . (, succ . snd . head $ ks) <$> foldMap (Set.toList . directDepsOn g . fst) (listToMaybe ks)) . (:[]) . (,1::Int)
    -- store and head cannot be called with empty lists.
    store (head -> (k,l)) = Map.insert k l
    seen (head -> (k,l)) = maybe False (< l) . Map.lookup k
    maximumOn :: Ord b => (a -> b) -> [a] -> a
    maximumOn f = maximumBy (comparing f)
