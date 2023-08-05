module Graphex (Input, getInput, reverseEdges, directDepsOn, allDepsOn, why, rankings) where

import           Algorithm.Search            (aStar)
import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.Aeson                  (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy        as BL
import           Data.Coerce                 (coerce)
import           Data.Map                    (Map)
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)

data Edge = Edge {
    from :: Text,
    to   :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass FromJSON

newtype Node = Node { label :: Text }
    deriving stock (Show, Generic)
    deriving anyclass FromJSON

data DepFile = DepFile {
    edges :: [Edge],
    nodes :: Map Text Node
    }
    deriving stock (Show, Generic)
    deriving anyclass FromJSON

type Input = Map Text [Text]

getInput :: FilePath -> IO Input
getInput fn = either fail (pure . resolve) . eitherDecode =<< BL.readFile fn
    where
        resolve DepFile{..} = Map.fromListWith (<>) [ (name from, [name to]) | Edge{..} <- edges]
            where
                name = (coerce nodes Map.!) :: Text -> Text

-- | Reverse all the arrows in the graphs.
reverseEdges :: Input -> Input
reverseEdges m = Map.fromListWith (<>) [ (v, [k]) | (k, vs) <- Map.assocs m, v <- k:vs ]

-- | Find the direct list of things that are referencing this module.
directDepsOn :: Input -> Text -> [Text]
directDepsOn = flip (Map.findWithDefault [])

-- Flood fill to find all transitive dependencies on a starting module.
allDepsOn :: Input -> Text -> [Text]
allDepsOn m k = Set.toList . Set.delete k . go mempty . Set.singleton $ k
    where
        go !s (flip Set.difference s -> todo)
            | Set.null todo = s
            | otherwise = go (s <> todo) (Set.unions $ Set.map (Set.fromList . directDepsOn m) todo)

-- | Find an example path between two modules.
--
-- This is a short path, but the important part is that it represents how connectivy works.
why :: Input -> Text -> Text -> [Text]
why m from to = maybe [] snd $ aStar (directDepsOn m) (const (const 1)) (const (1::Int)) (== to) from

-- | Count the number of transitive dependencies for each module.
rankings :: Input -> Map Text Int
rankings m = Map.fromList $ parMap rdeepseq (\k -> (k, length $ allDepsOn m k)) (Map.keys m)
