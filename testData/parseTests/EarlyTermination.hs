module EarlyTermination where

import Data.Text
import Data.Map

data Foo = Foo
  { bar :: Text
  , baz :: Int
  }

-- This import is after a data declaration and should NOT be parsed
import Data.List
