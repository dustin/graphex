{-# LANGUAGE StrictData #-}
module Graphex.LookingGlass where

import Data.Aeson qualified as Ae
import Data.String (IsString)
import GHC.Generics
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)

import Graphex.Core

data Node = Node
  { label :: Text
  , color :: Maybe Text
  } deriving stock (Show, Generic)
  deriving anyclass (Ae.FromJSON, Ae.ToJSON)

type NodeId = Text
data Edge = Edge
  { from :: NodeId
  , to :: NodeId
  } deriving (Show, Generic)
  deriving anyclass (Ae.FromJSON, Ae.ToJSON)

data GraphDef = GraphDef
  { title :: Text
  , nodes :: Map NodeId Node
  , edges :: [Edge]
  } deriving (Show, Generic)
  deriving anyclass (Ae.FromJSON, Ae.ToJSON)

newtype Color = Color { unColor :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

red :: Color
red = "red"

black :: Color
black = "black"

toLookingGlass
  :: Text -- ^ title
  -> Map ModuleName Color -- ^ Optionally re-color any nodes (black default)
  -> ModuleGraph
  -> GraphDef
toLookingGlass title colors ModuleGraph{..} =
  let mkNodeId (ModuleName m) = m
      mkNode m =
        ( mkNodeId m
        , Node
          { color = unColor <$> Map.lookup m colors
          , label = mkNodeId m
          }
        )
  in GraphDef
     { nodes = Map.fromList $ fmap (\(m, _) -> mkNode m) $ Map.toList imports
     , edges = Map.toList imports >>= \(m, children) -> fmap (\c -> Edge{from = mkNodeId c, to = mkNodeId m}) $ Set.toList children
     , ..
     }
