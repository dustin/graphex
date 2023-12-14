{-# LANGUAGE StrictData #-}
module Graphex.Diff where

import           Control.Arrow               (second)
import           Control.Parallel.Strategies (NFData)
import           Data.List                   (sortOn)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Monoid                 (Sum (..))
import           Data.Ord                    (Down (..))
import           Data.Semialign
import           Data.Text                   (Text)
import           Data.Tuple                  (swap)
import           Prelude                     hiding (div)
import           Text.Blaze.Html5

import           Graphex

data Diff a = Diff
  { netNodes         :: ~Int
  , nodes            :: ~(Map a Int)

  , netReversedNodes :: ~Int
  , reversedNodes    :: ~(Map a Int)
  } deriving stock (Show)

diff :: NFData a => Ord a => Graph a -> Graph a -> Diff a
diff g1 g2 = Diff{..}
  where
    g1rev = reverseEdges g1
    g2rev = reverseEdges g2

    r1 = rankings g1
    r2 = rankings g2

    r1rev = rankings g1rev
    r2rev = rankings g2rev

    doDiff x y =
      let x' = Map.fromList $ fmap (second (Sum . negate) . swap) x
          y' = Map.fromList $ fmap (second Sum . swap) y
      in Map.filter (/= 0) $ Map.map getSum $ salign x' y'

    nodes = doDiff r1 r2
    netNodes = sum nodes

    reversedNodes = doDiff r1rev r2rev
    netReversedNodes = sum reversedNodes

diff2html :: Diff Text -> Html
diff2html Diff{..} = mconcat
  [ div $ mconcat
    [ "Net change in transitive import dependencies: ", toHtml netNodes
    , details $ table $ mconcat
      [ tr $ mconcat
        [ th "Module", th "Change" ]
      , flip foldMap (sortOn (Down . abs . snd) $ Map.toList nodes) $ \(m, net) ->
          tr $ mconcat
          [ td $ toHtml m, td $ toHtml net ]
      ]
    ]
  ]
