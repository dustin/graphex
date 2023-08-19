module Graphex.CSV where

import Control.Monad ((>=>))
import GHC.Generics (Generic)
import Data.Csv qualified as CSV
import Data.Text (Text)
import Data.Map qualified as Map
import Data.Set qualified as Set

import Graphex.Core (Graph (..))

data Edge = Edge
  { from :: Text
  , to :: Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (CSV.DefaultOrdered, CSV.ToRecord, CSV.ToNamedRecord)

toEdges :: Graph -> [Edge]
toEdges = (Map.toList . unGraph) >=> \(from, tos) -> fmap (Edge from) (Set.toList tos)
