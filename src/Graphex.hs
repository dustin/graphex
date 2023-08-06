module Graphex (Graph(..), reverseEdges, directDepsOn, allDepsOn, why, rankings, restrictTo, export, depToGraph) where

import           Algorithm.Search            (dijkstra)
import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Coerce                 (coerce)
import           Data.Foldable               (fold, toList)
import           Data.Map                    (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           GHC.Generics                (Generic)

data Edge = Edge {
    from :: Text,
    to   :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype Node = Node { label :: Text }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data DepFile = DepFile {
    edges :: [Edge],
    nodes :: Map Text Node
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype Graph = Graph { unGraph :: Map Text (Set Text) }
    deriving stock (Eq)

depToGraph :: DepFile -> Graph
depToGraph DepFile{..} = Graph $ Map.fromListWith (<>) [ (name from, Set.singleton (name to)) | Edge{..} <- edges] <> Map.fromList [(label, mempty) | Node{..} <- Map.elems nodes]
    where
        name = (coerce nodes Map.!) :: Text -> Text

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

-- | Convert a graph back to our dependency file format.
export :: Graph -> DepFile
export (Graph m) = DepFile {
    edges = [ Edge { from = newKey k, to = newKey v } | (k, vs) <- Map.assocs m, v <- Set.toList vs ],
    nodes = Map.fromList [ (newKey k, Node k) | k <- Map.keys m ]
    }
    where
        allk = fold m <> Map.keysSet m
        newKey f = (Map.! f) . Map.fromList $ zip (toList allk) (T.pack . show <$> [0 :: Int ..])
