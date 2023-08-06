{-# LANGUAGE StrictData #-}
module Graphex.LookingGlass where

import Data.Aeson qualified as Ae
import Data.String (IsString)
import GHC.Generics
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Graphex.Core

data Node = Node
  { label :: Maybe String
  , color :: Maybe String
  } deriving stock (Show, Generic)
  deriving anyclass (Ae.FromJSON, Ae.ToJSON)

type NodeId = String
data Edge = Edge
  { from :: NodeId
  , to :: NodeId
  } deriving (Show, Generic)
  deriving anyclass (Ae.FromJSON, Ae.ToJSON)

data GraphDef = GraphDef
  { title :: String
  , nodes :: Map NodeId Node
  , edges :: [Edge]
  } deriving (Show, Generic)
  deriving anyclass (Ae.FromJSON, Ae.ToJSON)

newtype Color = Color { unColor :: String }
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

red :: Color
red = "red"

black :: Color
black = "black"

toLookingGlass
  :: String -- ^ title
  -> Map ModuleName Color -- ^ Optionally re-color any nodes (black default)
  -> ModuleGraph
  -> GraphDef
toLookingGlass title colors ModuleGraph{..} =
  let mkNodeId (ModuleName m) = T.unpack m
      mkNode m =
        ( mkNodeId m
        , Node
          { color = unColor <$> Map.lookup m colors
          , label = Nothing
          }
        )
  in GraphDef
     { nodes = Map.fromList $ fmap (\(m, _) -> mkNode m) $ Map.toList imports
     , edges = Map.toList imports >>= \(m, children) -> fmap (\c -> Edge{from = mkNodeId c, to = mkNodeId m}) $ Set.toList children
     , ..
     }
