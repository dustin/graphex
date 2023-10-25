{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

-- Cribbed from graphmod's 'Graphmod.CabalSupport'
module Graphex.Cabal
  ( discoverCabalModules
  , discoverCabalModuleGraph
  , CabalDiscoverOpts (..)
  , CabalDiscoverType (..)
  , CabalUnit (..)
  , CabalUnitType (..)
  , Discovery (..)
  , discoversUnit
  ) where

import           Graphex.Core
import           Graphex.Parser
import           Graphex.Queue

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (atomically)
import Control.Concurrent.QSem
import           Control.Monad                                 (guard)
import           Data.Foldable                                 (fold)
import           Data.List                                     (intersperse)
import           Data.List.NonEmpty                            (NonEmpty)
import qualified           Data.List.NonEmpty                            as NE
import           Data.Maybe                                    (maybeToList, mapMaybe)
import           Data.Semigroup.Foldable
import qualified Data.Set                                      as Set
import qualified Data.Map.Strict                               as Map
import           Data.String                                   (fromString)
import           System.Directory                              (doesFileExist,
                                                                getDirectoryContents)
import           System.FilePath                               (takeExtension,
                                                                (<.>), (</>))
import           UnliftIO.Async                                (pooledForConcurrentlyN,
                                                                pooledMapConcurrentlyN)

-- Interface to cabal.

import qualified Distribution.ModuleName                       as Cabal
import           Distribution.PackageDescription               (BuildInfo (..),
                                                                Executable (..),
                                                                Library (..),
                                                                PackageDescription (..),
                                                                TestSuite (..),
                                                                libraryNameString,
                                                                unUnqualComponentName)
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

discoverCabalModules :: CabalDiscoverOpts -> FilePath -> IO [Module]
discoverCabalModules CabalDiscoverOpts{..} cabalFile = do
  gpd <- readGenericPackageDescription silent cabalFile
  let PackageDescription{..} = flattenPackageDescription gpd
  let candidateModules = mconcat
        [ do
            Library{..} <- mconcat [maybeToList library, subLibraries]
            srcDir <- hsSourceDirs libBuildInfo
            exMod <- exposedModules
            let name = unUnqualComponentName <$> libraryNameString libName
            guard $ Discovered == foldMap1 (`discoversUnit` CabalLibraryUnit name) toDiscover
            pure Module
              { name = fromString $ mconcat $ intersperse "." $ Cabal.components exMod
              , path = ModuleFile $ sourceDirToFilePath srcDir </> Cabal.toFilePath exMod <.> ".hs"
              }
        , do
            Executable{..} <- executables
            srcDir <- hsSourceDirs buildInfo
            otherMod <- "Main" : buildInfo.otherModules
            guard $ Discovered == foldMap1 (`discoversUnit` CabalExecutableUnit (unUnqualComponentName exeName)) toDiscover
            pure Module
              { name = fromString $
                if otherMod == "Main"
                then unUnqualComponentName exeName ++ "-Main"
                else mconcat $ intersperse "." $ Cabal.components otherMod
              , path = ModuleFile $ sourceDirToFilePath srcDir </> Cabal.toFilePath otherMod <.> ".hs"
              }
        , do
            TestSuite{..} <- testSuites
            srcDir <- hsSourceDirs testBuildInfo
            otherMod <- testBuildInfo.otherModules
            guard $ Discovered == foldMap1 (`discoversUnit` CabalTestsUnit (unUnqualComponentName testName)) toDiscover

            pure Module
              { name = fromString $ mconcat $ intersperse "." $ Cabal.components otherMod
              , path = ModuleFile $ sourceDirToFilePath srcDir </> Cabal.toFilePath otherMod <.> ".hs"
              }
        ]

  pooledMapConcurrentlyN numJobs validateModulePath candidateModules

validateModulePath :: Module -> IO Module
validateModulePath m = do
  path <- case m.path of
    ModuleFile fp -> do
      fileExists <- doesFileExist fp
      pure $ if fileExists then ModuleFile fp else ModuleNoFile
    ModuleNoFile -> pure ModuleNoFile
  pure Module {name = m.name, path = path}

data CabalUnitType =
    CabalLibrary
  | CabalExecutable
  | CabalTests
  deriving stock (Show, Eq, Ord)

data CabalUnit =
    CabalLibraryUnit (Maybe String)
  | CabalExecutableUnit String
  | CabalTestsUnit String
  deriving stock (Show, Eq, Ord)

data CabalDiscoverType =
    CabalDiscoverAll CabalUnitType
  | CabalDiscover CabalUnit
  | CabalDontDiscoverAll CabalUnitType
  | CabalDontDiscover CabalUnit
  deriving stock (Show, Eq, Ord)

data Discovery =
    Discovered
  | Hidden
  | Passed
  deriving stock (Show, Eq, Ord)

instance Semigroup Discovery where
  _ <> Discovered = Discovered
  _ <> Hidden     = Hidden
  x <> Passed     = x

flipDiscover :: Discovery -> Discovery
flipDiscover = \case
  Passed -> Passed
  Hidden -> Discovered
  Discovered -> Hidden

discoversUnit :: CabalDiscoverType -> CabalUnit -> Discovery
discoversUnit = curry $ \case
  (CabalDiscoverAll CabalLibrary, CabalLibraryUnit{}) -> Discovered
  (CabalDiscoverAll CabalLibrary, _) -> Passed
  (CabalDiscoverAll CabalExecutable, CabalExecutableUnit{}) -> Discovered
  (CabalDiscoverAll CabalExecutable, _) -> Passed
  (CabalDiscoverAll CabalTests, CabalTestsUnit{}) -> Discovered
  (CabalDiscoverAll CabalTests, _) -> Passed
  (CabalDiscover u1, u2) -> if u1 == u2 then Discovered else Passed
  (CabalDontDiscoverAll ty, u) -> flipDiscover $ CabalDiscoverAll ty `discoversUnit` u
  (CabalDontDiscover u1, u2) -> flipDiscover $ CabalDiscover u1 `discoversUnit` u2

data CabalDiscoverOpts = CabalDiscoverOpts
  { toDiscover      :: NonEmpty CabalDiscoverType
  , includeExternal :: Bool
  , numJobs :: Int
  , pruneTo :: Maybe (NonEmpty ModuleName)
  } deriving stock (Show, Eq)

discoverCabalModuleGraph :: CabalDiscoverOpts -> IO ModuleGraph
discoverCabalModuleGraph opts@CabalDiscoverOpts{..} = do
  fs <- getDirectoryContents "."
  mods <- fmap fold . traverse (discoverCabalModules opts) . filter ((".cabal" ==) . takeExtension) $ fs

  let modMap = foldMap (\m@Module{..} -> Map.singleton name m) mods
  case pruneTo of
    Nothing -> do
      gs <- pooledForConcurrentlyN numJobs mods $ \Module{..} -> case path of
        ModuleFile modPath -> do
          allImps <- parseFileImports modPath
          let filteredImps = filter (\Import{..} -> Map.member module_ modMap) allImps
          pure $ mkModuleGraph name $ fmap module_ (if includeExternal then allImps else filteredImps)
        ModuleNoFile -> pure $ mkModuleGraph name mempty
      pure $ fold gs
    Just pruneModNames -> do
      sem <- newQSem numJobs
      graphRef <- newTVarIO @ModuleGraph mempty
      let go seen todo = case qpop todo of
            Nothing -> pure ()
            Just (Module{..}, rest) ->  case path of
              _ | Set.member name seen -> pure ()
              ModuleFile modPath -> do
                allImps <- parseFileImports modPath
                let filteredImps = filter (\Import{..} -> Map.member module_ modMap) allImps
                atomically $ modifyTVar' graphRef (\g -> mappend g $ mkModuleGraph name $ fmap module_ (if includeExternal then allImps else filteredImps))
                let importedMods = mapMaybe (\Import{..} -> Map.lookup module_ modMap) allImps
                go (Set.insert name seen) (qappendList rest importedMods)
              ModuleNoFile -> pure ()
      let pruneMods = mapMaybe (flip Map.lookup modMap) (NE.toList pruneModNames)
      go mempty (qlist pruneMods)
      readTVarIO graphRef
