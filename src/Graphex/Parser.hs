module Graphex.Parser where

import Data.Text qualified as T

import GHC.Parser qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import GHC.Hs qualified as GHC
import GHC.Unit.Module.Name qualified as GHC

import Graphex.Parser.GHC qualified as GHC
import Graphex.Core

parseModuleImports :: Module -> IO ModuleGraph
parseModuleImports Module{..} = GHC.runParserFile path GHC.parseModule >>= \case
  GHC.PFail _ -> error $ unwords ["Failed to parse module:", path]
  GHC.PSuccess (GHC.L _ a) -> case a of
    GHC.HsModule{..} ->
        pure
      $ mconcat
      $ fmap (singletonModuleGraph name)
      $ fmap (fromGHCModule . GHC.unLoc . GHC.ideclName . GHC.unLoc)
      $ hsmodImports

fromGHCModule :: GHC.ModuleName -> ModuleName
fromGHCModule  = ModuleName . T.pack . GHC.moduleNameString
