{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData          #-}
module Graphex.Core where

import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.String     (IsString)
import           Data.Text       (Text)

newtype Graph = Graph { unGraph :: Map Text (Set Text) }
    deriving stock (Eq)

-- Haskell
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
  }
  deriving stock (Show, Eq)

singletonModuleGraph :: ModuleName -> ModuleName -> ModuleGraph
singletonModuleGraph importer importee = ModuleGraph
  { imports = Map.singleton importer (Set.singleton importee)
  }

mkModuleGraph :: ModuleName -> [ModuleName] -> ModuleGraph
mkModuleGraph importer importees = ModuleGraph
  { imports = Map.singleton importer (Set.fromList importees) }

instance Semigroup ModuleGraph where
  x <> y = ModuleGraph
    { imports = Map.unionWith (<>) x.imports y.imports
    }

instance Monoid ModuleGraph where
  mempty = ModuleGraph mempty
