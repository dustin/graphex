module Graphex (Input, getInput, reverseEdges, directDepsOn, allDepsOn, why, rankings, restrictTo, export) where

import           Algorithm.Search            (dijkstra)
import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.Aeson                  (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy        as BL
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

type Input = Map Text (Set Text)

getInput :: FilePath -> IO Input
getInput fn = either fail (pure . resolve) . eitherDecode =<< BL.readFile fn
    where
        resolve DepFile{..} = Map.fromListWith (<>) [ (name from, Set.singleton (name to)) | Edge{..} <- edges]
            where
                name = (coerce nodes Map.!) :: Text -> Text

-- | Reverse all the arrows in the graphs.
reverseEdges :: Input -> Input
reverseEdges m = Map.fromListWith (<>) [ (v, Set.singleton k) | (k, vs) <- Map.assocs m, v <- Set.toList vs ] <> Map.fromSet (const mempty) (Map.keysSet m)

-- | Find the direct list of things that are referencing this module.
directDepsOn :: Input -> Text -> Set Text
directDepsOn = flip (Map.findWithDefault mempty)

-- Flood fill to find all transitive dependencies on a starting module.
allDepsOn :: Input -> Text -> Set Text
allDepsOn m k = Set.delete k . go mempty . Set.singleton $ k
    where
        go !s (flip Set.difference s -> todo)
            | Set.null todo = s
            | otherwise = go (s <> todo) (Set.unions $ Set.map (directDepsOn m) todo)

-- | Find an example path between two modules.
--
-- This is a short path, but the important part is that it represents how connectivy works.
why :: Input -> Text -> Text -> [Text]
why m from to = maybe [] snd $ dijkstra (directDepsOn m) (const (const (1::Int))) (== to) from

-- | Count the number of transitive dependencies for each module.
rankings :: Input -> Map Text Int
rankings m = Map.fromList $ parMap rdeepseq (\k -> (k, length $ allDepsOn m k)) (Map.keys m)

-- | Restrict a graph to only the modules that reference a given module.
restrictTo :: Input -> Text -> Input
restrictTo g k = flip Map.mapMaybeWithKey g $ \k' v -> if k' == k then Just v else nonNullSet (Set.intersection keep v)
    where
        keep = allDepsOn g k
        nonNullSet v = if Set.null v then Nothing else Just v

-- | Convert a graph back to our dependency file format.
export :: Input -> DepFile
export g = DepFile {
    edges = [ Edge { from = newKey k, to = newKey v } | (k, vs) <- Map.assocs g, v <- Set.toList vs ],
    nodes = Map.fromList [ (newKey k, Node k) | k <- Map.keys g ]
    }
    where
        allk = fold g <> Map.keysSet g
        newKey f = (Map.! f) . Map.fromList $ zip (toList allk) (T.pack . show <$> [0 :: Int ..])
