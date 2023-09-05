{-# LANGUAGE ApplicativeDo #-}
module Main where

import           Control.Applicative  ((<|>))
import           Data.Aeson           (encode)
import           Data.Bool            (bool)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as CSV
import           Data.Foldable
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import           Data.Maybe           (fromMaybe)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Tree.View       (drawTree)
import           Options.Applicative  (Parser, argument, command,
                                       customExecParser, fullDesc, help, helper,
                                       hsubparser, info, long, metavar, prefs,
                                       progDesc, short, showDefault,
                                       showHelpOnError, some, str, strOption,
                                       switch, value, (<**>))

import           Graphex
import           Graphex.Cabal
import           Graphex.Core
import qualified Graphex.CSV
import           Graphex.LookingGlass

data Command
    = DirectDepsOn Text
    | AllDepsOn (NonEmpty Text)
    | Why { fromModule :: Text, toModule :: Text, showAll :: Bool }
    | AllPaths Text Text
    | Rankings
    | FindLongest
    | Select Text
    | ToCSV Bool
    deriving stock Show


data Options = GraphCmd GraphOptions | CabalCmd CabalOptions
  deriving stock Show

data GraphOptions = GraphOptions {
    optGraph   :: FilePath,
    optReverse :: Bool,
    optCommand :: Command
    } deriving stock Show

options :: Parser Options
options = hsubparser $ fold
  [ command "graph" (info (GraphCmd <$> graphOptions) (progDesc "Graph operations"))
  , command "cabal" (info (CabalCmd <$> cabalOptions) (progDesc "Cabal operations"))
  ]

data CabalOptions = CabalOptions
  { optDiscoverExes    :: Bool
  , optDiscoverTests   :: Bool
  , optIncludeExternal :: Bool
  } deriving stock Show

cabalOptions :: Parser CabalOptions
cabalOptions = do
  optDiscoverExes <- switch (long "discover-exes" <> help "Discover exe import dependencies")
  optDiscoverTests <- switch (long "discover-tests" <> help "Discover test import dependencies")
  optIncludeExternal <- switch (long "include-external" <> help "Include external import dependencies")
  pure CabalOptions{..}

graphOptions :: Parser GraphOptions
graphOptions = GraphOptions
    <$> strOption (long "graph" <> short 'g' <> showDefault <> value "graph.json" <> help "path to graph data")
    <*> switch (long "reverse" <> short 'r' <> help "reverse edges")
    <*> hsubparser (fold [
        command "deps" (info depsCmd (progDesc "Show all direct inbound dependencies to a module")),
        command "all" (info allDepsCmd (progDesc "Show all dependencies to a module")),
        command "why" (info whyCmd (progDesc "Show why a module depends on another module")),
        command "all-paths" (info allPathsCmd (progDesc "Show all ways a module depends on another module")),
        command "rank" (info (pure Rankings) (progDesc "Show the most depended on modules")),
        command "longest" (info (pure FindLongest) (progDesc "Show the longest shortest path between two modules")),
        command "select" (info selectCmd (progDesc "Select a subset of the graph from a starting module")),
        command "to-csv" (info csvCmd (progDesc "Convert to a CSV of edges compatible with SQLite and Gephi"))
        ])

    where
        depsCmd = DirectDepsOn <$> argument str (metavar "module")
        allDepsCmd = AllDepsOn <$> some1 (argument str (metavar "module"))
        selectCmd = Select <$> argument str (metavar "module")
        whyCmd = Why <$> argument str (metavar "module") <*> argument str (metavar "module") <*> switch (long "all" <> help "Show all paths")
        allPathsCmd = AllPaths <$> argument str (metavar "module from") <*> argument str (metavar "module to")
        csvCmd = ToCSV <$> switch (long "no-header" <> short 'n' <> help "Omit CSV header")
        some1 = fmap NE.fromList . some

printStrs :: Foldable f => f Text -> IO ()
printStrs = traverse_ TIO.putStrLn

main :: IO ()
main = customExecParser (prefs showHelpOnError) opts >>= \case
  GraphCmd GraphOptions{..} -> do
    let handleReverse = bool id reverseEdges optReverse
    graph <- handleReverse <$> getInput optGraph
    case optCommand of
        Why{..}      ->
          if showAll
          then
              drawTree
            $ fmap T.unpack
            $ graphToTree fromModule
            $ allPathsTo graph fromModule toModule
          else
            let explainer = if optReverse then " imports " else " imported by "
                mkWhy rev = uncurry (why graph) $ if rev then (toModule, fromModule) else (fromModule, toModule)
                renderWhy rev = NE.zipWith (<>) ("" :| repeat explainer) <$> mkWhy rev
            in printStrs $ fromMaybe (pure "No path found") $ renderWhy True <|> renderWhy False
        AllPaths from to -> BL.putStr . encode . graphToDep . (setAttribute from "note" "start" . setAttribute to "note" "end") $ allPathsTo graph from to
        DirectDepsOn m   -> printStrs $ directDepsOn graph m
        AllDepsOn m      -> printStrs $ foldMap (allDepsOn graph) m
        Rankings         -> printStrs $ fmap (\(n,m) -> m <> " - " <> (T.pack . show) n) $ rankings graph
        FindLongest      -> printStrs $ longest graph
        Select m         -> BL.putStr $ encode (graphToDep (handleReverse (setAttribute m "note" "start" $ restrictTo graph (allDepsOn graph m))))
        ToCSV noHeader   -> BL.putStr $ (if noHeader then CSV.encode else CSV.encodeDefaultOrderedByName) $ Graphex.CSV.toEdges graph
  CabalCmd CabalOptions{..} -> do
    mg <- discoverCabalModuleGraph CabalDiscoverOpts
      { toDiscover = mconcat
          [ Set.singleton CabalLibraries
          , bool mempty (Set.singleton CabalExecutables) optDiscoverExes
          , bool mempty (Set.singleton CabalTests) optDiscoverTests
          ]
      , includeExternal = optIncludeExternal
      }
    BL.putStr $ encode $ toLookingGlass "Internal Package Dependencies" mempty mg
  where
    opts = info (options <**> helper) ( fullDesc <> progDesc "Graph CLI tool.")
