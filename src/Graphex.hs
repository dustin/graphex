module Graphex (Input, getInput, reverseEdges, directDepsOn, allDepsOn, why) where

import           Algorithm.Search
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as BL
import           Data.Map             (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe)
import           Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Tuple           (swap)
import           GHC.Generics         (Generic)

-- I let Aeson parse this in a way that fits the structure and then just rewrite it to usable types.
data Input' e n = Input {
    edges :: e,
    nodes :: n
    }
    deriving stock (Show, Generic)
    deriving anyclass FromJSON

instance Bifunctor Input' where
    bimap f g Input{..} = Input (f edges) (g nodes)

data Edge = Edge {
    from :: Text,
    to   :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass FromJSON

data Node = Node { label :: Text }
    deriving stock (Show, Generic)
    deriving anyclass FromJSON

type Input = Input' (Map Int [Int]) (Int -> Text, Text -> Maybe Int)

getInput :: FilePath -> IO Input
getInput fn = either fail (pure . bimap pedge pnode) . eitherDecode =<< BL.readFile fn
    where
        pedge :: [Edge] -> Map Int [Int]
        pedge = Map.fromListWith (<>) . fmap (\Edge{..} -> (tread from, [tread to]))

        pnode :: Map Text Node -> (Int -> Text, Text -> Maybe Int)
        pnode = flipAndReverse . Map.foldMapWithKey (\k v -> Map.singleton (tread k) (label v))
        flipAndReverse m = ((m Map.!), flip Map.lookup (Map.fromList . fmap swap $ Map.assocs m))

        tread = read . T.unpack

reverseEdges :: Input -> Input
reverseEdges i@Input{..} = i{ edges = Map.fromListWith (<>) [ (v, [k]) | (k, vs) <- Map.assocs edges, v <- vs ] }

nameOf :: Input -> Int -> Text
nameOf Input{..} = fst nodes

named :: Input -> Text -> Maybe Int
named Input{..} = snd nodes

directDepsOn :: Input -> Text -> [Text]
directDepsOn i@Input{..} = fmap (nameOf i) . maybe [] (flip (Map.findWithDefault []) edges) . named i

-- Flood fill to find all transitive dependencies from a starting module.
allDepsOn :: Input -> Text -> [Text]
allDepsOn i@Input{..} = fmap (nameOf i) . maybe [] (Set.toList . go mempty . Set.singleton) . named i
    where
        nf x = Set.fromList $ Map.findWithDefault [] x edges

        go s (flip Set.difference s -> todo)
            | Set.null todo = s
            | otherwise = go (s <> todo) (Set.unions $ Set.map nf todo)

-- Find an example path between two modules.
why :: Input -> Text -> Text -> [Text]
why i@Input{..} from to = fromMaybe [] $ do
    from' <- named i from
    to' <- named i to
    (_, resolved) <- aStar nf (const (const 1)) (const (1::Int)) (== to') from'
    pure $ fmap (nameOf i) resolved

    where
        nf x = Map.findWithDefault [] x edges
