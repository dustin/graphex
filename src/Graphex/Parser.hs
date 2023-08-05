module Graphex.Parser where

import Data.Text (Text)

import GHC.Parser qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import GHC.Hs qualified as GHC

import Graphex.Parser.GHC qualified as GHC

newtype ModuleName = ModuleName { unModuleName :: Text }

data Import = Import
  { moduleName :: ModuleName
  , identifiers :: [Text]
  }

parseFileImports :: FilePath -> IO [Import]
parseFileImports path = GHC.runParserFile path GHC.parseModule >>= \case
  GHC.PFail _ -> error $ unwords ["Failed to parse module:", path]
  GHC.PSuccess (GHC.L _ a) -> case a of
    GHC.HsModule{..} -> undefined
