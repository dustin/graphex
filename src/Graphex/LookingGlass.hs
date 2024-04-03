{-# LANGUAGE StrictData #-}
module Graphex.LookingGlass where

import           Data.Aeson      (withObject, (.!=), (.:), (.:?))
import qualified Data.Aeson      as Ae
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.String     (IsString)
import           Data.Text       (Text)
import           GHC.Generics

import           Graphex.Core

data Node = Node
  { label :: Text
  , color :: Maybe Text
  } deriving stock (Show, Generic)
  deriving anyclass (Ae.FromJSON, Ae.ToJSON)

type NodeId = Text
data Edge = Edge
  { from :: NodeId
  , to   :: NodeId
  } deriving (Show, Generic)
  deriving anyclass (Ae.FromJSON, Ae.ToJSON)

data GraphDef = GraphDef
  { title :: Text
  , nodes :: Map NodeId Node
  , edges :: [Edge]
  , attrs :: Map NodeId (Map Text Text)
  } deriving (Show, Generic)
  deriving anyclass Ae.ToJSON

instance Ae.FromJSON GraphDef where
  parseJSON = withObject "GraphDef" $ \o -> do
    title <- o .: "title"
    nodes <- o .: "nodes"
    edges <- o .: "edges"
    attrs <- o .:? "attrs" .!= mempty
    pure GraphDef{..}

newtype Color = Color { unColor :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

red :: Color
red = "red"

black :: Color
black = "black"

toLookingGlass
  :: Text -- ^ title
  -> Map Text Color -- ^ Optionally re-color any nodes (black default)
  -> Graph Text
  -> GraphDef
toLookingGlass title colors Graph{..} =
  let mkNodeId m = m
      mkNode m =
        ( mkNodeId m
        , Node
          { color = unColor <$> Map.lookup m colors
          , label = mkNodeId m
          }
        )
  in GraphDef
     { nodes = Map.fromList $ concatMap (\(m, children) -> mkNode m : fmap mkNode (Set.toList children) ) $ Map.toList unGraph
     , edges = Map.toList unGraph >>= \(m, children) -> fmap (\c -> Edge{to = mkNodeId c, from = mkNodeId m}) $ Set.toList children
     , attrs = attributes
     , ..
     }
