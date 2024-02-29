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
import qualified Data.Text                   as T
import           Data.Tuple                  (swap)
import           Hable
import           Prelude                     hiding (div)
import           Text.Blaze.Html5

import           Graphex

data Diff a = Diff
  { netNodes         :: Int
  , nodes            :: Map a Int

  , netReversedNodes :: Int
  , reversedNodes    :: Map a Int
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

diffFoldFor :: Ord a => Monoid b => (Diff a -> Map a Int) -> Diff a -> ((a, Int) -> b) -> b
diffFoldFor getter d = flip foldMap (sortOn (Down . abs . snd) $ Map.toList $ getter d)

diff2html :: Diff Text -> Html
diff2html d = mconcat
  [ div $ mconcat
    [ "Net change in transitive import dependencies: ", toHtml (netNodes d)
    , details $ table $ mconcat
      [ tr $ mconcat
        [ th "Module", th "Change" ]
      , diffFoldFor nodes d $ \(m, net) ->
          tr $ mconcat
          [ td $ toHtml m, td $ toHtml net ]
      ]
    ]
  , div $ mconcat
    [ "Net change in reverse transitive import dependencies: ", toHtml (netReversedNodes d)
    , details $ table $ mconcat
      [ tr $ mconcat
        [ th "Module", th "Change" ]
      , diffFoldFor reversedNodes d $ \(m, net) ->
          tr $ mconcat
          [ td $ toHtml m, td $ toHtml net ]
      ]
    ]
  ]

diff2text :: Diff Text -> Text
diff2text d = T.pack $ unlines
  [ unwords ["Net change in transitive import dependencies:", show (netNodes d)]
  , hable defaultConfig $ (diffHeader :) $ diffFoldFor nodes d $ \(m, net) ->
      [[T.unpack m, show net]]
  , unwords ["Net change in reverse transitive import dependencies:", show (netReversedNodes d)]
  , hable defaultConfig $ (diffHeader :) $ diffFoldFor reversedNodes d $ \(m, net) ->
      [[T.unpack m, show net]]
  ]

diffHeader :: [String]
diffHeader = ["Module", "Change"]
