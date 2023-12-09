module Graphex.UnionMap where

import Data.Map

newtype UnionMap k v = UnionMap { unUnionMap :: (Map k v) }

instance (Ord k, Semigroup v) => Semigroup (UnionMap k v) where
  UnionMap x <> UnionMap y = UnionMap $ unionWith (<>) x y

instance (Ord k, Semigroup v) => Monoid (UnionMap k v) where
  mempty = UnionMap mempty