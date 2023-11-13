{-# LANGUAGE StrictData #-}
module Graphex.Core where

import           Control.Monad   ((<=<))
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.String     (IsString (..))
import           Data.Text       (Text)

data Graph a = Graph {
  unGraph    :: Map a (Set a),
  attributes :: Map a (Map Text Text)
  }
  deriving stock Eq

instance Ord a => Semigroup (Graph a) where
  Graph x xa <> Graph y ya = Graph (Map.unionWith (<>) x y) (Map.unionWith (<>) xa ya)

instance Ord a => Monoid (Graph a) where
  mempty = Graph mempty mempty

singletonGraph :: Ord a => a -> a -> Graph a
singletonGraph k = mkGraph k . pure

mkGraph :: Ord a => a -> [a] -> Graph a
mkGraph k links = Graph (Map.singleton k (Set.fromList links)) mempty

setAttribute :: Ord a => a -> Text -> Text -> Graph a -> Graph a
setAttribute k attr val g@(Graph _ attrs) = g{attributes = Map.insertWith (<>) k (Map.singleton attr val) attrs}

getAttribute :: Ord a => a -> Text -> Graph a -> Maybe Text
getAttribute nodeName aName = (Map.lookup aName <=< Map.lookup nodeName) . attributes

graphNodes :: Graph a -> [a]
graphNodes = Map.keys . unGraph

-- Haskell
data Import = Import
  { module_ :: ModuleName
  , package :: Maybe Text
  } deriving stock (Show, Eq)

newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving newtype (Eq, Ord, IsString)
  deriving stock (Show)

data ModulePath = ModuleNoFile | ModuleFile FilePath
  deriving stock (Eq, Ord, Show)

moduleFilePath :: Module -> Maybe FilePath
moduleFilePath Module{..} = case path of
  ModuleNoFile  -> Nothing
  ModuleFile fp -> Just fp

instance IsString ModulePath where
  fromString = ModuleFile

data Module = Module
  { name :: ModuleName
  , path :: ModulePath
  }
  deriving stock (Show, Eq, Ord)

type ModuleGraph = Graph ModuleName

singletonModuleGraph :: ModuleName -> ModuleName -> ModuleGraph
singletonModuleGraph = singletonGraph

mkModuleGraph :: ModuleName -> [ModuleName] -> ModuleGraph
mkModuleGraph = mkGraph

-- | Convert a Graph of type @a@ to a Graph of type @b@.
-- This is like a functor, but it can't be a functor because of laws and stuff.
convertGraph :: Ord b => (a -> b) -> Graph a -> Graph b
convertGraph f (Graph m attrs) = g{attributes = Map.mapKeys f attrs}
    where g = foldMap (\(k, vs) -> Graph (Map.singleton (f k) (Set.map f vs)) mempty) (Map.assocs m)
