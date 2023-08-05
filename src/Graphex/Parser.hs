module Graphex.Parser where

import Data.Text (Text)
import Data.Text qualified as T

import GHC.Parser qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import GHC.Hs qualified as GHC
import GHC.Unit.Module.Name qualified as GHC

import Graphex.Parser.GHC qualified as GHC

newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving newtype (Eq, Ord)
  deriving stock (Show)

data Import = Import
  { moduleName :: ModuleName
  } deriving stock (Show)

parseFileImports :: FilePath -> IO [Import]
parseFileImports path = GHC.runParserFile path GHC.parseModule >>= \case
  GHC.PFail _ -> error $ unwords ["Failed to parse module:", path]
  GHC.PSuccess (GHC.L _ a) -> case a of
    GHC.HsModule{..} -> pure $ fmap (Import . fromGHCModule . GHC.unLoc . GHC.ideclName . GHC.unLoc) $ hsmodImports

fromGHCModule :: GHC.ModuleName -> ModuleName
fromGHCModule  = ModuleName . T.pack . GHC.moduleNameString
