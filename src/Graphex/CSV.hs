module Graphex.CSV where

import           Control.Monad ((>=>))
import qualified Data.Csv      as CSV
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.Text     (Text)
import           GHC.Generics  (Generic)

import           Graphex.Core  (Graph (..))

data Edge = Edge
  { from :: Text
  , to   :: Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (CSV.DefaultOrdered, CSV.ToRecord, CSV.ToNamedRecord)

toEdges :: Graph Text -> [Edge]
toEdges = (Map.toList . unGraph) >=> \(from, tos) -> fmap (Edge from) (Set.toList tos)
