{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- Cribbed from graphmod's 'Graphmod.CabalSupport'
module Graphex.Cabal
  ( discoverCabalModules
  , discoverCabalModuleGraph
  , CabalDiscoverOpts (..)
  , CabalModuleType (..)
  ) where

import           Graphex.Core
import           Graphex.Parser

import           Control.Monad                                 (guard)
import           Data.Foldable                                 (fold)
import           Data.List                                     (intersperse)
import           Data.Maybe                                    (maybeToList)
import           Data.Set                                      (Set)
import qualified Data.Set                                      as Set
import           Data.String                                   (fromString)
import           System.Directory                              (doesFileExist, getDirectoryContents)
import           System.FilePath                               (takeExtension, (<.>), (</>))
import           UnliftIO.Async                                (pooledForConcurrently, pooledMapConcurrently)

-- Interface to cabal.

import qualified Distribution.ModuleName                       as Cabal
import           Distribution.PackageDescription               (BuildInfo (..), Executable (..), Library (..),
                                                                PackageDescription (..), TestSuite (..),
                                                                unUnqualComponentName)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Verbosity                        (silent)

#if MIN_VERSION_Cabal(3,6,0)
import           Distribution.Utils.Path                       (PackageDir, SourceDir, SymbolicPath, getSymbolicPath)
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

discoverCabalModules :: CabalDiscoverOpts -> FilePath -> IO [Module]
discoverCabalModules CabalDiscoverOpts{..} cabalFile = do
  gpd <- readGenericPackageDescription silent cabalFile
  let PackageDescription{..} = flattenPackageDescription gpd
  let candidateModules = mconcat
        [ do
            guard $ Set.member CabalLibraries toDiscover
            Library{..} <- mconcat [maybeToList library, subLibraries]
            srcDir <- hsSourceDirs libBuildInfo
            exMod <- exposedModules
            pure Module
              { name = fromString $ mconcat $ intersperse "." $ Cabal.components exMod
              , path = ModuleFile $ sourceDirToFilePath srcDir </> Cabal.toFilePath exMod <.> ".hs"
              }
        , do
            guard $ Set.member CabalExecutables toDiscover
            Executable{..} <- executables
            srcDir <- hsSourceDirs buildInfo
            otherMod <- "Main" : buildInfo.otherModules
            pure Module
              { name = fromString $
                if otherMod == "Main"
                then unUnqualComponentName exeName ++ "-Main"
                else mconcat $ intersperse "." $ Cabal.components otherMod
              , path = ModuleFile $ sourceDirToFilePath srcDir </> Cabal.toFilePath otherMod <.> ".hs"
              }
        , do
            guard $ Set.member CabalTests toDiscover
            TestSuite{..} <- testSuites
            srcDir <- hsSourceDirs testBuildInfo
            otherMod <- testBuildInfo.otherModules
            pure Module
              { name = fromString $ mconcat $ intersperse "." $ Cabal.components otherMod
              , path = ModuleFile $ sourceDirToFilePath srcDir </> Cabal.toFilePath otherMod <.> ".hs"
              }
        ]

  pooledMapConcurrently validateModulePath candidateModules

validateModulePath :: Module -> IO Module
validateModulePath m = do
  path <- case m.path of
    ModuleFile fp -> do
      fileExists <- doesFileExist fp
      pure $ if fileExists then ModuleFile fp else ModuleNoFile
    ModuleNoFile -> pure ModuleNoFile
  pure Module {name = m.name, path = path}

data CabalModuleType =
  CabalLibraries
  | CabalExecutables
  | CabalTests
  deriving stock (Show, Eq, Ord)

data CabalDiscoverOpts = CabalDiscoverOpts
  { toDiscover      :: Set CabalModuleType
  , includeExternal :: Bool
  } deriving stock (Show, Eq)

discoverCabalModuleGraph :: CabalDiscoverOpts -> IO ModuleGraph
discoverCabalModuleGraph opts@CabalDiscoverOpts{..} = do
  fs <- getDirectoryContents "." -- XXX
  mods <- fmap fold . pooledMapConcurrently (discoverCabalModules opts) . filter ((".cabal" ==) . takeExtension) $ fs

  let modSet = foldMap (Set.singleton . name) mods
  gs <- pooledForConcurrently mods $ \Module{..} -> case path of
    ModuleFile modPath -> do
      allImps <- parseFileImports modPath
      let filteredImps =
            if includeExternal
            then allImps
            else filter (\Import{..} -> Set.member module_ modSet) allImps
      pure $ mkModuleGraph name $ fmap module_ filteredImps
    ModuleNoFile -> pure $ mkModuleGraph name mempty
  pure $ fold gs
