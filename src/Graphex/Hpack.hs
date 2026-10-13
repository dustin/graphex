{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE StrictData          #-}

module Graphex.Hpack
  ( discoverHpackModules
  , discoverHpackModuleGraph
  , HpackDiscoverOpts (..)
  , pathToModuleName
  , findHsFiles
  ) where

import           Graphex.Cabal (CabalDiscoverType (..), CabalGraph (..),
                                CabalUnit (..), Discovery (..),
                                buildModuleGraph, discoversUnit)
import           Graphex.Core

import           Data.Aeson             (FromJSON (..), Value (..), withObject,
                                         (.:?))
import           Data.Aeson.Types       (Parser, typeMismatch)
import           Data.Either            (partitionEithers)
import           Data.List              (isSuffixOf)
import           Data.List.NonEmpty     (NonEmpty)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Semigroup.Foldable
import qualified Data.Set               as Set
import qualified Data.Text              as T
import           Data.Text              (Text)
import qualified Data.Vector            as V
import           Data.Yaml.Include      (decodeFileEither)
import           System.Directory       (doesDirectoryExist, doesFileExist,
                                         listDirectory)
import           System.FilePath        (normalise, (</>))
import           UnliftIO.Async         (pooledMapConcurrentlyN)

-- | Top-level package.yaml structure
data PackageYaml = PackageYaml
  { pyLibrary           :: Maybe ComponentConfig
  , pyInternalLibraries :: Map Text ComponentConfig
  , pyExecutables       :: Map Text ExecutableConfig
  , pyTests             :: Map Text ExecutableConfig
  }

data ComponentConfig = ComponentConfig
  { ccSourceDirs     :: V.Vector Text
  , ccExposedModules :: Maybe (V.Vector Text)
  , ccOtherModules   :: Maybe (V.Vector Text)
  }

data ExecutableConfig = ExecutableConfig
  { ecSourceDirs    :: V.Vector Text
  , ecMain          :: Maybe Text
  , ecOtherModules  :: Maybe (V.Vector Text)
  }

-- | Handles hpack's flexible source-dirs: can be a single string or a list
parseSourceDirs :: Value -> Parser (V.Vector Text)
parseSourceDirs (String s) = pure (V.singleton s)
parseSourceDirs (Array a)  = traverse parseJSON a
parseSourceDirs v          = typeMismatch "String or Array" v

instance FromJSON PackageYaml where
  parseJSON = withObject "PackageYaml" $ \o -> do
    pyLibrary <- o .:? "library"
    pyInternalLibraries <- fromMaybe mempty <$> o .:? "internal-libraries"
    pyExecutables <- fromMaybe mempty <$> o .:? "executables"
    pyTests <- fromMaybe mempty <$> o .:? "tests"
    pure PackageYaml{..}

instance FromJSON ComponentConfig where
  parseJSON = withObject "ComponentConfig" $ \o -> do
    rawDirs <- o .:? "source-dirs"
      >>= traverse parseSourceDirs
    let ccSourceDirs = fromMaybe (V.singleton ".") rawDirs
    ccExposedModules <- o .:? "exposed-modules"
    ccOtherModules <- o .:? "other-modules"
    pure ComponentConfig{..}

instance FromJSON ExecutableConfig where
  parseJSON = withObject "ExecutableConfig" $ \o -> do
    rawDirs <- o .:? "source-dirs"
      >>= traverse parseSourceDirs
    let ecSourceDirs = fromMaybe (V.singleton ".") rawDirs
    ecMain <- o .:? "main"
    ecOtherModules <- o .:? "other-modules"
    pure ExecutableConfig{..}

data HpackDiscoverOpts = HpackDiscoverOpts
  { hpackToDiscover      :: NonEmpty CabalDiscoverType
  , hpackIncludeExternal :: Bool
  , hpackNumJobs         :: Int
  , hpackPruneTo         :: Maybe (ModuleName -> Bool)
  }

-- | Convert a file path relative to a source directory into a module name.
-- e.g. pathToModuleName "src" "src/Graphex/Core.hs" == "Graphex.Core"
pathToModuleName :: FilePath -> FilePath -> ModuleName
pathToModuleName srcDir fp = pathToModuleNameText (T.pack srcDir) (T.pack fp)

-- | Text-based version that avoids String operations entirely.
-- Strips the source dir prefix, drops ".hs", replaces '/' with '.'.
pathToModuleNameText :: Text -> Text -> ModuleName
pathToModuleNameText srcDir fp =
    ModuleName $ T.map (\c -> if c == '/' || c == '\\' then '.' else c) relative
  where
    -- Strip "srcDir/" prefix, then drop ".hs" suffix
    stripped = case T.stripPrefix srcDir fp of
      Just rest -> T.dropWhile (== '/') rest
      Nothing   -> fp
    relative = fromMaybe stripped $ T.stripSuffix ".hs" stripped

-- | Recursively find all .hs files under a directory,
-- excluding specified subdirectories.
findHsFiles :: Int -> FilePath -> IO [FilePath]
findHsFiles numJobs dir = findHsFilesExcluding numJobs dir Set.empty

findHsFilesExcluding :: Int -> FilePath -> Set.Set FilePath -> IO [FilePath]
findHsFilesExcluding numJobs dir excludeDirs = do
  exists <- doesDirectoryExist dir
  if not exists then pure [] else go dir
  where
    go d = do
      entries <- listDirectory d
      (hsFiles, subDirs) <- partitionEntries d entries
      subResults <- pooledMapConcurrentlyN numJobs go subDirs
      pure $ hsFiles <> concat subResults

    partitionEntries d entries = do
      classified <- pooledMapConcurrentlyN numJobs (classifyEntry d) entries
      let merge (fs, ds) (afs, ads) =
            (fs <> afs, ds <> ads)
      pure $ foldr merge ([], []) classified

    classifyEntry d entry = do
      let full = d </> entry
      isDir <- doesDirectoryExist full
      pure $ if isDir
        then if normalise full `Set.member` excludeDirs
             then ([], [])
             else ([], [full])
        else (if ".hs" `isSuffixOf` entry then [full] else [], [])

-- | Spec for globbing a source directory
data GlobSpec = GlobSpec
  { gsSrcDir       :: Text
  , gsExcludeDirs  :: Set.Set FilePath  -- subdirs that are also source dirs
  , gsExcludeFiles :: Set.Set FilePath  -- files to skip
  }

-- | Convert a Text module name to a relative file path (as Text).
-- e.g. "Graphex.Core" -> "Graphex/Core.hs"
moduleNameToRelPath :: Text -> Text
moduleNameToRelPath modName = T.replace "." "/" modName <> ".hs"

-- | Join a directory and filename as Text using "/"
(</+>) :: Text -> Text -> Text
dir </+> file = dir <> "/" <> file
infixr 5 </+>

-- | Discover modules from a package.yaml file.
-- Uses explicit module lists when available,
-- falls back to globbing source dirs.
discoverHpackModules :: HpackDiscoverOpts -> FilePath -> IO [Module]
discoverHpackModules HpackDiscoverOpts{..} yamlFile = do
  pkg <- either (fail . show) pure =<< decodeFileEither yamlFile
  let results = discoverLibrary Nothing (pyLibrary pkg)
        <> discoverInternalLibraries (pyInternalLibraries pkg)
        <> discoverExecutables (pyExecutables pkg)
        <> discoverTests (pyTests pkg)

  let (dirsToGlob, explicitModules) = partitionEithers (V.toList results)

  -- Validate explicit modules (check files exist)
  validated <- catMaybes <$> pooledMapConcurrentlyN hpackNumJobs
    validateModule explicitModules

  -- Glob source directories for .hs files
  globbed <- fmap concat $ pooledMapConcurrentlyN hpackNumJobs
    globSourceDir dirsToGlob

  pure $ validated <> globbed

  where

    shouldDiscover :: CabalUnit -> Bool
    shouldDiscover unit =
      Discovered == foldMap1 (`discoversUnit` unit) hpackToDiscover

    validateModule :: Module -> IO (Maybe Module)
    validateModule m@Module{path} = case path of
      ModuleFile fp -> do
        exists <- doesFileExist fp
        pure $ if exists then Just m else Nothing
      ModuleNoFile -> pure $ Just m

    globSourceDir :: GlobSpec -> IO [Module]
    globSourceDir GlobSpec{..} = do
      let srcDirFP = T.unpack gsSrcDir
      hsFiles <- findHsFilesExcluding hpackNumJobs srcDirFP gsExcludeDirs
      let notExcluded f =
            normalise f `Set.notMember` gsExcludeFiles
          toModule f = Module
            { name = pathToModuleNameText gsSrcDir (T.pack f)
            , path = ModuleFile f
            }
      pure $ map toModule $ filter notExcluded hsFiles

    -- | Compute exclude dirs: for each source dir, exclude other source dirs
    -- that are proper subdirectories of it.
    mkGlobSpecs
      :: V.Vector Text
      -> V.Vector Text
      -> V.Vector (Either GlobSpec Module)
    mkGlobSpecs excludeFiles srcDirs =
      V.map mkOne srcDirs
      where
        toNorm = normalise . T.unpack
        excludeFileSet = Set.fromList $
          map toNorm (V.toList excludeFiles)
        mkOne sd = Left GlobSpec
          { gsSrcDir = sd
          , gsExcludeDirs = Set.fromList $
              map toNorm $
              filter (isProperSubdirOf sd) $
              V.toList srcDirs
          , gsExcludeFiles = excludeFileSet
          }

    isProperSubdirOf :: Text -> Text -> Bool
    isProperSubdirOf parent child =
        let p = parent <> "/"
        in child /= parent && p `T.isPrefixOf` child

    discoverLibrary
      :: Maybe Text
      -> Maybe ComponentConfig
      -> V.Vector (Either GlobSpec Module)
    discoverLibrary _ Nothing = mempty
    discoverLibrary libName (Just ComponentConfig{..})
      | not (shouldDiscover (CabalLibraryUnit (T.unpack <$> libName))) = mempty
      | Just exposed <- ccExposedModules =
          let others = fromMaybe mempty ccOtherModules
              allMods = exposed <> others
          in V.concatMap (modulesFromExplicitV ccSourceDirs) allMods
      | otherwise = mkGlobSpecs mempty ccSourceDirs

    discoverInternalLibraries
      :: Map Text ComponentConfig
      -> V.Vector (Either GlobSpec Module)
    discoverInternalLibraries = Map.foldMapWithKey $ \n cfg ->
        discoverLibrary (Just n) (Just cfg)

    discoverExecutables
      :: Map Text ExecutableConfig
      -> V.Vector (Either GlobSpec Module)
    discoverExecutables =
      Map.foldMapWithKey $ \n ExecutableConfig{..} ->
        if not (shouldDiscover (CabalExecutableUnit (T.unpack n)))
        then mempty else
        let mainFile = fromMaybe "Main.hs" ecMain
            mainPath = fromMaybe "." (ecSourceDirs V.!? 0) </+> mainFile
            mainMod = V.singleton $ Right Module
              { name = ModuleName (n <> "-Main")
              , path = ModuleFile (T.unpack mainPath)
              }
            otherMods = case ecOtherModules of
              Just others ->
                V.concatMap (modulesFromExplicitV ecSourceDirs) others
              Nothing ->
                mkGlobSpecs (V.singleton mainPath) ecSourceDirs
        in mainMod <> otherMods

    discoverTests
      :: Map Text ExecutableConfig
      -> V.Vector (Either GlobSpec Module)
    discoverTests =
      Map.foldMapWithKey $ \n ExecutableConfig{..} ->
        if not (shouldDiscover (CabalTestsUnit (T.unpack n)))
        then mempty else
        let mainFile = fromMaybe "Main.hs" ecMain
            mainPath = fromMaybe "." (ecSourceDirs V.!? 0) </+> mainFile
        in case ecOtherModules of
          Just others ->
            V.concatMap (modulesFromExplicitV ecSourceDirs) others
          Nothing ->
            mkGlobSpecs (V.singleton mainPath) ecSourceDirs

    -- | Create Right Module candidates for an
    -- explicit module name across source dirs.
    modulesFromExplicitV
      :: V.Vector Text
      -> Text
      -> V.Vector (Either GlobSpec Module)
    modulesFromExplicitV srcDirs modName
      | "Paths_" `T.isPrefixOf` modName =
          V.singleton $ Right Module
            { name = ModuleName modName
            , path = ModuleNoFile
            }
      | otherwise =
          let relPath = moduleNameToRelPath modName
          in V.map (\sd -> Right Module
                 { name = ModuleName modName
                 , path = ModuleFile $ T.unpack (sd </+> relPath)
                 }) srcDirs

discoverHpackModuleGraph :: HpackDiscoverOpts -> FilePath -> IO CabalGraph
discoverHpackModuleGraph opts@HpackDiscoverOpts{..} yamlFile = do
  mods <- discoverHpackModules opts yamlFile
  buildModuleGraph hpackNumJobs hpackIncludeExternal hpackPruneTo mods
