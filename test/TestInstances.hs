module TestInstances () where

import           Data.Foldable (fold)
import qualified Data.Map      as Map
import qualified Data.Set      as Set

import           Graphex.Core

-- It's usually a terrible idea to write your own Show instance, but this is just for debugging.
instance Show t => Show (Graph t) where
    show (Graph m attrs) = fold [show k <> " -> " <> show (Set.toList vs) <> ", " | (k, vs) <- Map.assocs m] <> " attrs=" <> show attrs
