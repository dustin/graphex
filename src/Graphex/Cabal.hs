{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- Cribbed from graphmod's 'Graphmod.CabalSupport'
module Graphex.Cabal (discoverCabalModules, discoverCabalModuleGraph) where

import           Graphex.Core
import           Graphex.Parser

import           Control.Monad                                 (filterM)
import           Data.Foldable                                 (fold)
import           Data.List                                     (intersperse)
import           Data.Maybe                                    (maybeToList)
import qualified Data.Set                                      as Set
import           Data.String                                   (fromString)
import           Data.Traversable                              (for)
import           System.Directory                              (doesFileExist,
                                                                getDirectoryContents)
import           System.FilePath                               (takeExtension,
                                                                (<.>), (</>))

-- Interface to cabal.
import qualified Distribution.ModuleName                       as Cabal
import           Distribution.PackageDescription               (BuildInfo (..),
                                                                Library (..),
                                                                PackageDescription (..))
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Verbosity                        (silent)

#if MIN_VERSION_Cabal(3,6,0)
import           Distribution.Utils.Path                       (PackageDir,
                                                                SourceDir,
                                                                SymbolicPath,
                                                                getSymbolicPath)
#endif

#if MIN_VERSION_Cabal(3,8,1)
import           Distribution.Simple.PackageDescription        (readGenericPackageDescription)
#elif MIN_VERSION_Cabal(2,2,0)
import           Distribution.PackageDescription.Parsec        (readGenericPackageDescription)
#else
import           Distribution.PackageDescription.Parse         (readGenericPackageDescription)
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
  mods <- fmap fold . traverse discoverCabalModules . filter ((".cabal" ==) . takeExtension) $ fs

  let modSet = foldMap (Set.singleton . name) mods
  gs <- for mods $ \Module{..} -> do
    internalImps <- filter (\Import{..} -> Set.member module_ modSet) <$> parseFileImports path
    pure $ mkModuleGraph name $ fmap module_ internalImps
  pure $ fold gs
