module FunctionSigTermination where

import Data.Text
import Data.Map
import Data.Set

foo :: Text -> Map Text (Set Int)
foo = undefined

-- This import is after a function signature and should NOT be parsed
import Data.List
