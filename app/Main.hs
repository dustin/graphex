module Main where

import           Data.Aeson           (eitherDecode, encode)
import           Data.Bool            (bool)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable
import           Data.List            (sortOn)
import qualified Data.Map.Strict      as Map
import           Data.Ord             (Down (..))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Options.Applicative  (Parser, argument, command, customExecParser, fullDesc, help, helper, hsubparser,
                                       info, long, metavar, prefs, progDesc, short, showDefault, showHelpOnError, str,
                                       strOption, switch, value, (<**>))

import           Graphex

data Command
    = DirectDepsOn Text
    | AllDepsOn Text
    | Why Text Text
    | Rankings
    | Select Text
    deriving stock Show

data Options = Options {
    optGraph   :: FilePath,
    optReverse :: Bool,
    optCommand :: Command
    } deriving stock Show

options :: Parser Options
options = Options
    <$> strOption (long "graph" <> short 'g' <> showDefault <> value "graph.json" <> help "path to graph data")
    <*> switch (long "reverse" <> short 'r' <> help "reverse edges")
    <*> hsubparser (fold [
        command "deps" (info depsCmd (progDesc "Show all direct inbound dependencies to a module")),
        command "all" (info allDepsCmd (progDesc "Show all dependencies to a module")),
        command "why" (info whyCmd (progDesc "Show why a module depends on another module")),
        command "rank" (info (pure Rankings) (progDesc "Show the most depended on modules")),
        command "select" (info selectCmd (progDesc "Select a subset of the graph from a starting module"))
        ])

    where
        depsCmd = DirectDepsOn <$> argument str (metavar "module")
        allDepsCmd = AllDepsOn <$> argument str (metavar "module")
        selectCmd = Select <$> argument str (metavar "module")
        whyCmd = Why <$> argument str (metavar "module from") <*> argument str (metavar "module to")

printStrs :: Foldable f => f Text -> IO ()
printStrs = traverse_ TIO.putStrLn

getInput :: FilePath -> IO Graph
getInput fn = either fail (pure . depToGraph) . eitherDecode =<< BL.readFile fn

main :: IO ()
main = do
    Options{..} <- customExecParser (prefs showHelpOnError) opts
    graph <- bool id reverseEdges optReverse <$> getInput optGraph
    case optCommand of
        Why from to    -> printStrs $ why graph from to
        DirectDepsOn m -> printStrs $ directDepsOn graph m
        AllDepsOn m    -> printStrs $ allDepsOn graph m
        Rankings       -> printStrs $ fmap (\(m,n) -> m <> " - " <> (T.pack . show) n) . sortOn (Down . snd) . Map.assocs $ rankings graph
        Select m       -> BL.putStr $ encode (graphToDep (restrictTo graph m))
  where
    opts = info (options <**> helper) ( fullDesc <> progDesc "Graph CLI tool.")
