module Graphex.Logger (logit) where

import           Debug.Trace        (traceM)
import           System.Environment (lookupEnv)
import Data.Functor ((<&>))

data Verbosity =
    Silent
  | Verbose

verbosityFromEnv :: IO Verbosity
verbosityFromEnv = lookupEnv "GRAPHEX_VERBOSITY" <&> \case
  Nothing -> Silent
  Just{} -> Verbose

logit :: String -> IO ()
logit str = verbosityFromEnv >>= \case
  Silent -> pure ()
  Verbose -> traceM str
