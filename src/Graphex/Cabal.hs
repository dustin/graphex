{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeApplications    #-}

-- Originally cribbed from graphmod's 'Graphmod.CabalSupport'
module Graphex.Cabal
  ( discoverCabalModules
  , discoverCabalModuleGraph
  , CabalDiscoverOpts (..)
  , CabalDiscoverType (..)
  , CabalUnit (..)
  , CabalUnitType (..)
  , Discovery (..)
  , discoversUnit
  , CabalGraph (..)
  , mkCabalFileGraph
  ) where

import           Graphex.Core
import           Graphex.Logger
import           Graphex.Parser

import           Control.Concurrent.QSem
import           Control.Concurrent.STM                        (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad                                 (guard)
import           Data.Foldable                                 (fold)
import           Data.List                                     (intersperse)
import           Data.List.NonEmpty                            (NonEmpty)
import           Data.Map.Strict                               (Map)
import qualified Data.Map.Strict                               as Map
import           Data.Maybe                                    (catMaybes,
                                                                mapMaybe,
                                                                maybeToList)
import           Data.Semigroup.Foldable
import qualified Data.Set                                      as Set
import           Data.String                                   (fromString)
import qualified Data.Text                                     as T
import           System.Directory                              (doesFileExist,
                                                                getDirectoryContents)
import           System.FilePath                               (takeExtension,
                                                                (<.>), (</>))
import           UnliftIO.Async                                (mapConcurrently_,
                                                                pooledForConcurrentlyN,
                                                                pooledMapConcurrentlyN)
import           UnliftIO.Exception                            (bracket_)

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
              , path = if exMod `elem` libBuildInfo.autogenModules then ModuleNoFile else ModuleFile $ sourceDirToFilePath srcDir </> Cabal.toFilePath exMod <.> ".hs"
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
              , path = if otherMod `elem` buildInfo.autogenModules then ModuleNoFile else ModuleFile $ sourceDirToFilePath srcDir </> Cabal.toFilePath otherMod <.> ".hs"
              }
        , do
            TestSuite{..} <- testSuites
            srcDir <- hsSourceDirs testBuildInfo
            otherMod <- testBuildInfo.otherModules
            guard $ Discovered == foldMap1 (`discoversUnit` CabalTestsUnit (unUnqualComponentName testName)) toDiscover

            pure Module
              { name = fromString $ mconcat $ intersperse "." $ Cabal.components otherMod
              , path = if otherMod `elem` testBuildInfo.autogenModules then ModuleNoFile else ModuleFile $ sourceDirToFilePath srcDir </> Cabal.toFilePath otherMod <.> ".hs"
              }
        ]

  catMaybes <$> pooledMapConcurrentlyN numJobs validateModulePath candidateModules

validateModulePath :: Module -> IO (Maybe Module)
validateModulePath m = do
  let mkModule p = Module m.name p
  case m.path of
    ModuleFile fp -> do
      fileExists <- doesFileExist fp
      -- If a discovered module has a filepath but doesn't exist, it means
      -- that the unit had multiple source directories. We assume it will
      -- exist in one of those directories.
      pure $ if fileExists then Just $ mkModule $ ModuleFile fp else Nothing
    ModuleNoFile -> pure $ Just $ mkModule ModuleNoFile

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
  , numJobs         :: Int
  , pruneTo         :: Maybe (ModuleName -> Bool)
  }

data CabalGraph = CabalGraph
  { moduleGraph   :: ModuleGraph
  , modulesByName :: Map ModuleName Module
  }

discoverCabalModuleGraph :: CabalDiscoverOpts -> IO CabalGraph
discoverCabalModuleGraph opts@CabalDiscoverOpts{..} = do
  fs <- getDirectoryContents "."
  mods <- fmap fold . traverse (discoverCabalModules opts) . filter ((".cabal" ==) . takeExtension) $ fs

  let modMap = foldMap (\m@Module{..} -> Map.singleton name m) mods
  mg <- case pruneTo of
    Nothing -> do
      gs <- pooledForConcurrentlyN numJobs mods $ \Module{..} -> case path of
        ModuleFile modPath -> do
          allImps <- parseFileImports modPath
          let filteredImps = filter (\Import{..} -> Map.member module_ modMap) allImps
          pure $ mkModuleGraph name $ fmap module_ (if includeExternal then allImps else filteredImps)
        ModuleNoFile -> pure $ mkModuleGraph name mempty
      pure $ fold gs
    Just keep -> do
      sem <- newQSem numJobs
      graphRef <- newTVarIO @ModuleGraph mempty
      seenRef <- newTVarIO @(Set.Set ModuleName) mempty
      let go Module{..} = case path of
            ModuleFile modPath -> do
              importedMods <- bracket_ (waitQSem sem) (signalQSem sem) $ do
                seen <- (Set.member name) <$> readTVarIO seenRef
                if seen then pure [] else do
                  logit $ unwords ["Start parsing imports for", T.unpack $ unModuleName name]
                  allImps <- parseFileImports modPath
                  logit $ unwords ["Finished parsing imports for", T.unpack $ unModuleName name]
                  let filteredImps = filter (\Import{..} -> Map.member module_ modMap) allImps
                  atomically $ modifyTVar' graphRef (\g -> mappend g $ mkModuleGraph name $ fmap module_ (if includeExternal then allImps else filteredImps))
                  atomically $ modifyTVar' seenRef (Set.insert name)
                  pure $ mapMaybe (\Import{..} -> Map.lookup module_ modMap) allImps
              mapConcurrently_ go importedMods
            ModuleNoFile -> pure ()
      let pruneToMods = fmap snd $ filter (keep . fst) (Map.toList modMap)
      logit $ unwords ["Pruning graph to transitive imports of:", show $ fmap (unModuleName . name) pruneToMods]
      mapConcurrently_ go pruneToMods
      readTVarIO graphRef
  pure CabalGraph
    { moduleGraph = mg
    , modulesByName = modMap
    }

mkCabalFileGraph :: CabalGraph -> Graph FilePath
mkCabalFileGraph CabalGraph{..} =
    Graph
    { unGraph = Map.fromList $ flip mapMaybe (Map.toList $ unGraph moduleGraph) $ \(p, cs) -> do
        p' <- Map.lookup p modulesByName >>= moduleFilePath
        let cs' = Set.fromList $ mapMaybe (\c -> Map.lookup c modulesByName >>= moduleFilePath) $ Set.toList cs
        Just (p', cs')
    , attributes = Map.fromList $ flip mapMaybe (Map.toList $ attributes moduleGraph) $ \(n, as) -> do
        n' <- Map.lookup n modulesByName >>= moduleFilePath
        Just (n', as)
    }
