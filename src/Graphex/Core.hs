{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
module Graphex.Core where

import Data.String (IsString)
import           Data.Map                    (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)

data Import = Import
  { module_ :: ModuleName
  , package :: Maybe Text
  } deriving stock (Show)

newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving newtype (Eq, Ord, IsString)
  deriving stock (Show)

data Module = Module
  { name :: ModuleName
  , path :: FilePath
  }
  deriving stock (Show)

data ModuleGraph = ModuleGraph
  { imports :: Map ModuleName (Set ModuleName)
  , reverseImports :: Map ModuleName (Set ModuleName)
  }
  deriving stock (Show)

singletonModuleGraph :: ModuleName -> ModuleName -> ModuleGraph
singletonModuleGraph importer importee = ModuleGraph
  { imports = Map.singleton importer (Set.singleton importee)
  , reverseImports = Map.singleton importee (Set.singleton importer)
  }

instance Semigroup ModuleGraph where
  x <> y = ModuleGraph
    { imports = Map.unionWith (<>) x.imports y.imports
    , reverseImports = Map.unionWith (<>) x.reverseImports y.reverseImports
    }

instance Monoid ModuleGraph where
  mempty = ModuleGraph mempty mempty
