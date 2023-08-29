{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData          #-}
module Graphex.Core where

import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.String     (IsString)
import           Data.Text       (Text)

newtype Graph a = Graph { unGraph :: Map a (Set a) }
  deriving stock Eq

instance Ord a => Semigroup (Graph a) where
  Graph x <> Graph y = Graph (Map.unionWith (<>) x y)

instance Ord a => Monoid (Graph a) where
  mempty = Graph mempty

singletonGraph :: Ord a => a -> a -> Graph a
singletonGraph k = mkGraph k . pure

mkGraph :: Ord a => a -> [a] -> Graph a
mkGraph k = Graph . Map.singleton k . Set.fromList

-- Haskell
data Import = Import
  { module_ :: ModuleName
  , package :: Maybe Text
  } deriving stock (Show)

newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving newtype (Eq, Ord, IsString)
  deriving stock (Show)

data Module = Module
  { name :: ModuleName
  , path :: FilePath
  }
  deriving stock (Show)

type ModuleGraph = Graph ModuleName

singletonModuleGraph :: ModuleName -> ModuleName -> ModuleGraph
singletonModuleGraph = singletonGraph

mkModuleGraph :: ModuleName -> [ModuleName] -> ModuleGraph
mkModuleGraph = mkGraph

-- | Convert a Graph of type @a@ to a Graph of type @b@.
-- This is like a functor, but it can't be a functor because of laws and stuff.
convertGraph :: Ord b => (a -> b) -> Graph a -> Graph b
convertGraph f = foldMap (\(k, vs) -> Graph (Map.singleton (f k) (Set.map f vs))) . Map.assocs . unGraph
