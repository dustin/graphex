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
import Data.Traversable (for)
import System.FilePath((</>), (<.>), takeExtension)
import System.Directory (doesFileExist, getDirectoryContents)

-- Interface to cabal.
import Distribution.Verbosity(silent)
import Distribution.PackageDescription
        ( PackageDescription(..)
        , Library(..), Executable(..)
        , BuildInfo(..), TestSuite(..)
        , unUnqualComponentName )
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
            Library{..} <- mconcat [maybeToList library, subLibraries]
            srcDir <- hsSourceDirs libBuildInfo
            exMod <- exposedModules
            pure Module
              { name = fromString $ mconcat $ intersperse "." $ Cabal.components exMod
              , path = sourceDirToFilePath srcDir </> Cabal.toFilePath exMod <.> ".hs"
              }
        , do
            Executable{..} <- executables
            srcDir <- hsSourceDirs buildInfo
            otherMod <- "Main" : buildInfo.otherModules
            pure Module
              { name = fromString $
                if otherMod == "Main"
                then unUnqualComponentName exeName ++ "-Main"
                else mconcat $ intersperse "." $ Cabal.components otherMod
              , path = sourceDirToFilePath srcDir </> Cabal.toFilePath otherMod <.> ".hs"
              }
        , do
            TestSuite{..} <- testSuites
            srcDir <- hsSourceDirs testBuildInfo
            otherMod <- testBuildInfo.otherModules
            pure Module
              { name = fromString $ mconcat $ intersperse "." $ Cabal.components otherMod
              , path = sourceDirToFilePath srcDir </> Cabal.toFilePath otherMod <.> ".hs"
              }
        ]

  filterM (doesFileExist . (.path)) candidateModules

discoverCabalModuleGraph :: IO ModuleGraph
discoverCabalModuleGraph = do
  fs <- getDirectoryContents "." -- XXX
  mods <- case filter ((".cabal" ==) . takeExtension) fs of
    path : _ -> discoverCabalModules path
    _ -> error "No cabal file found"

  let modSet = Set.fromList $ fmap (.name) mods
  gs <- for mods $ \Module{..} -> do
    imps <- parseFileImports path
    let internalImps = filter (\Import{..} -> Set.member module_ modSet) imps
    pure $ mkModuleGraph name $ fmap (.module_) internalImps
  pure $ mconcat gs
