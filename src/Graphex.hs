module Graphex (Input, getInput, reverseEdges, directDepsOn, allDepsOn, why) where

import           Algorithm.Search
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Map             (Map)
import qualified Data.Map.Strict      as Map
import           Data.Set             as Set
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

data Edge = Edge {
    from :: Text,
    to   :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass FromJSON

newtype Node = Node { label :: Text }
    deriving stock (Show, Generic)
    deriving anyclass FromJSON

data DepFile e n = DepFile {
    edges :: [Edge],
    nodes :: Map Text Node
    }
    deriving stock (Show, Generic)
    deriving anyclass FromJSON

type Input = Map Text [Text]

getInput :: FilePath -> IO Input
getInput fn = either fail (pure . resolve) . eitherDecode =<< BL.readFile fn
    where
        resolve DepFile{..} = Map.fromListWith (<>) [ (names Map.! from, [names Map.! to]) | Edge{..} <- edges]
            where
                names = fmap label nodes

-- | Reverse all the arrows in the graphs.
reverseEdges :: Input -> Input
reverseEdges m = Map.map (const []) m <> Map.fromListWith (<>) [ (v, [k]) | (k, vs) <- Map.assocs m, v <- vs ]

-- | Find the direct list of things that are referencing this module.
directDepsOn :: Input -> Text -> [Text]
directDepsOn m = flip (Map.findWithDefault []) m

-- Flood fill to find all transitive dependencies on a starting module.
allDepsOn :: Input -> Text -> [Text]
allDepsOn m k = Set.toList . Set.delete k . go mempty . Set.singleton $ k
    where
        nf x = Set.fromList $ Map.findWithDefault [] x m

        go s (flip Set.difference s -> todo)
            | Set.null todo = s
            | otherwise = go (s <> todo) (Set.unions $ Set.map nf todo)

-- | Find an example path between two modules.
--
-- This is a short path, but the important part is that it represents how connectivy works.
why :: Input -> Text -> Text -> [Text]
why m from to = maybe [] snd $ aStar nf (const (const 1)) (const (1::Int)) (== to) from
    where
        nf x = Map.findWithDefault [] x m
