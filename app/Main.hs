{-# LANGUAGE ApplicativeDo #-}
module Main where

import           Control.Monad            (when)
import           Data.Aeson               (encode)
import           Data.Bool                (bool)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Csv                 as CSV
import           Data.Foldable
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Data.Tree.View           (drawTree)
import           Options.Applicative
import           System.IO                (stdin)
import           Text.Blaze.Renderer.Utf8 (renderMarkup)
import           Text.Regex.TDFA

import           Graphex
import           Graphex.Core
import qualified Graphex.CSV
import           Graphex.Diff
import           Main.Cabal

data Command
    = DirectDepsOn Text
    | AllDepsOn { useRegex :: Bool, patterns :: (NonEmpty Text) }
    | Why { fromModule :: Text, toModule :: Text, showAll :: Bool }
    | AllPaths Text Text
    | Rankings
    | FindLongest
    | Select Text
    | ToCSV Bool
    | Cat (NonEmpty FilePath)
    | Remove { useRegex :: Bool, patterns :: (NonEmpty Text) }
    deriving stock Show


data Options = GraphCmd GraphOptions | CabalCmd CabalOptions | DiffCmd DiffOptions
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
  , command "diff" (info (DiffCmd <$> diffOptions) (progDesc "Diff operations"))
  ]

graphOptions :: Parser GraphOptions
graphOptions = GraphOptions
    <$> strOption (long "graph" <> short 'g' <> showDefault <> value "graph.json" <> help "Path to graph data. Use - for stdin.")
    <*> switch (long "reverse" <> short 'r' <> help "reverse edges")
    <*> hsubparser (fold [
        command "deps" (info depsCmd (progDesc "Show all direct inbound dependencies to a module")),
        command "all" (info allDepsCmd (progDesc "Show all dependencies to a module")),
        command "why" (info whyCmd (progDesc "Show why a module depends on another module")),
        command "all-paths" (info allPathsCmd (progDesc "Show all ways a module depends on another module")),
        command "rank" (info (pure Rankings) (progDesc "Show the most depended on modules")),
        command "longest" (info (pure FindLongest) (progDesc "Show the longest shortest path between two modules")),
        command "select" (info selectCmd (progDesc "Select a subset of the graph from a starting module")),
        command "to-csv" (info csvCmd (progDesc "Convert to a CSV of edges compatible with SQLite and Gephi")),
        command "cat" (info catCmd (progDesc "Concatenate multiple graphs")),
        command "remove" (info removeCmd (progDesc "Remove nodes from the graph"))
        ])

    where
        depsCmd = DirectDepsOn <$> argument str (metavar "module")
        allDepsCmd = AllDepsOn <$> switch (short 'r' <> help "Use regex") <*> some1 (argument str (metavar "module"))
        selectCmd = Select <$> argument str (metavar "module")
        whyCmd = Why <$> argument str (metavar "module") <*> argument str (metavar "module") <*> switch (long "all" <> help "Show all paths")
        allPathsCmd = AllPaths <$> argument str (metavar "module from") <*> argument str (metavar "module to")
        csvCmd = ToCSV <$> switch (long "no-header" <> short 'n' <> help "Omit CSV header")
        catCmd = Cat <$> some1 (argument str (metavar "graph.json"))
        removeCmd = Remove <$> switch (short 'r' <> help "Use regex") <*> some1 (argument str (metavar "module"))
        some1 = fmap NE.fromList . some

data DiffType =
    DiffRanks
  | DiffReverseRanks
  deriving stock (Show)
data DiffOptions = DiffOptions
  { graph1 :: FilePath
  , graph2 :: FilePath
  , format :: DiffFormat
  }
  deriving stock (Show)

data DiffFormat =
    DiffText
  | DiffHtml
  deriving stock (Show)

readDiffFormat :: ReadM DiffFormat
readDiffFormat = eitherReader $ \case
  "text" -> Right DiffText
  "html" -> Right DiffHtml
  x -> Left $ "Unrecognized diff format: " ++ x

diffOptions :: Parser DiffOptions
diffOptions = DiffOptions
  <$> argument str (metavar "GRAPH")
  <*> argument str (metavar "GRAPH")
  <*> option readDiffFormat (long "format" <> short 'f')

printStrs :: Foldable f => f Text -> IO ()
printStrs = traverse_ TIO.putStrLn

main :: IO ()
main = customExecParser (prefs showHelpOnError) opts >>= \case
  GraphCmd GraphOptions{..} -> do
    let handleReverse = bool id reverseEdges optReverse
    graph <- if | optGraph == "-" -> handleReverse <$> hGetInput stdin
                | otherwise       -> handleReverse <$> getInput optGraph
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
        AllDepsOn{..}      -> do
          let ms =
                if | useRegex -> filter (\m -> any (m =~) patterns) (graphNodes graph)
                   | otherwise -> filter (flip Map.member (unGraph graph)) $ NE.toList patterns
          when (null ms) $ error $ unwords ["No nodes in the graph match:", show patterns]
          printStrs $ foldMap (allDepsOn graph) ms
        Rankings         -> printStrs $ fmap (\(n,m) -> m <> " - " <> (T.pack . show) n) $ rankings graph
        FindLongest      -> printStrs $ longest graph
        Select m         -> BL.putStr $ encode (graphToDep (handleReverse (setAttribute m "note" "start" $ restrictTo graph (allDepsOn graph m))))
        ToCSV noHeader   -> BL.putStr $ (if noHeader then CSV.encode else CSV.encodeDefaultOrderedByName) $ Graphex.CSV.toEdges graph
        Cat files        -> BL.putStr . encode . graphToDep . fold =<< traverse getInput files
        Remove{..} -> do
          let shouldRemove =
                if | useRegex  -> \m -> any (m =~) patterns
                   | otherwise -> (`elem` patterns)
          BL.putStr $ encode $ graphToDep $ filterNodes shouldRemove graph
  CabalCmd cabalOpts -> runCabal cabalOpts
  DiffCmd DiffOptions{..} -> do
    g1 <- getInput graph1
    g2 <- getInput graph2
    let Diff{..} = diff g1 g2
    case format of
      DiffHtml -> BL.putStr $ renderMarkup $ diff2html Diff{..}
      DiffText -> do
        TIO.putStrLn $ diff2text Diff{..}
  where
    opts = info (options <**> helper) ( fullDesc <> progDesc "Graph CLI tool.")
