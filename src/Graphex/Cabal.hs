{-# language CPP #-}
{-# language OverloadedStrings #-}
{-# language OverloadedRecordDot #-}

-- Cribbed from graphmod's 'Graphmod.CabalSupport'
module Graphex.Cabal (discoverCabalModules, discoverCabalModuleGraph) where

import Graphex.Core
import Graphex.Parser

import Control.Monad (filterM)
import Data.String (fromString)
import Data.List (intersperse)
import Data.Maybe(maybeToList)
import Data.Set qualified as Set
import System.FilePath((</>), (<.>), takeExtension)
import System.Directory (doesFileExist, getDirectoryContents)

-- Interface to cabal.
import Distribution.Verbosity(silent)
import Distribution.PackageDescription
        ( PackageDescription(..)
        , Library(..), Executable(..)
        , BuildInfo(..) )
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.ModuleName qualified as Cabal

#if MIN_VERSION_Cabal(3,6,0)
import Distribution.Utils.Path (SymbolicPath, PackageDir, SourceDir, getSymbolicPath)
#endif

#if MIN_VERSION_Cabal(3,8,1)
import Distribution.Simple.PackageDescription(readGenericPackageDescription)
#elif MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec(readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse(readGenericPackageDescription)
#endif

-- Note that this isn't nested under the above #if because we need
-- the backwards-compatible version to be available for all Cabal
-- versions prior to 3.6
#if MIN_VERSION_Cabal(3,6,0)
sourceDirToFilePath :: SymbolicPath PackageDir SourceDir -> FilePath
sourceDirToFilePath = getSymbolicPath
#else
sourceDirToFilePath :: FilePath -> FilePath
sourceDirToFilePath = id
#endif

discoverCabalModules :: FilePath -> IO [Module]
discoverCabalModules cabalFile = do
  gpd <- readGenericPackageDescription silent cabalFile
  let PackageDescription{..} = flattenPackageDescription gpd
  let candidateModules = mconcat
        [ do
            Library{..} <- maybeToList library
            srcDir <- hsSourceDirs libBuildInfo
            exMod <- exposedModules
            pure Module
              { name = fromString $ mconcat $ intersperse "." $ Cabal.components exMod
              , path = sourceDirToFilePath srcDir </> Cabal.toFilePath exMod <.> ".hs"
              }
        ] -- TODO: exes + other-modules

  filterM (doesFileExist . (.path)) candidateModules

discoverCabalModuleGraph :: IO ModuleGraph
discoverCabalModuleGraph = do
  fs <- getDirectoryContents "." -- XXX
  mods <- case filter ((".cabal" ==) . takeExtension) fs of
    path : _ -> discoverCabalModules path
    _ -> error "No cabal file found"
  imps <- mconcat <$> traverse parseModuleImports mods
  let modSet = Set.fromList $ fmap (.name) mods
  let internalImps = filter (\(x, y) -> Set.member x modSet && Set.member y modSet) imps
  pure $ foldMap (uncurry singletonModuleGraph) internalImps
