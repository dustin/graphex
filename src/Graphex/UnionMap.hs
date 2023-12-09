module Graphex.UnionMap where

import           Data.Map.Strict

newtype UnionMap k v = UnionMap { unUnionMap :: (Map k v) }
  deriving stock (Foldable)

instance (Ord k, Semigroup v) => Semigroup (UnionMap k v) where
  UnionMap x <> UnionMap y = UnionMap $ unionWith (<>) x y

instance (Ord k, Semigroup v) => Monoid (UnionMap k v) where
  mempty = UnionMap mempty
