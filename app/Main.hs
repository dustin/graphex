module Main where

import           Data.Bool           (bool)
import           Data.Foldable
import           Data.List           (sort)
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Options.Applicative (Parser, argument, command,
                                      customExecParser, fullDesc, help, helper,
                                      hsubparser, info, long, metavar, prefs,
                                      progDesc, short, showDefault,
                                      showHelpOnError, str, strOption, switch,
                                      value, (<**>))

import           Graphex

data Command
    = DirectDepsOn Text
    | AllDepsOn Text
    | Why Text Text
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
        command "why" (info whyCmd (progDesc "Show why a module depends on another module"))])

    where
        depsCmd = DirectDepsOn <$> argument str (metavar "module")
        allDepsCmd = AllDepsOn <$> argument str (metavar "module")
        whyCmd = Why <$> argument str (metavar "module from") <*> argument str (metavar "module to")

main :: IO ()
main = do
    Options{..} <- customExecParser (prefs showHelpOnError) opts
    graph <- bool id reverseEdges optReverse <$> getInput optGraph
    traverse_ TIO.putStrLn $ case optCommand of
        Why from to    -> why graph from to
        DirectDepsOn m -> sort $ directDepsOn graph m
        AllDepsOn m    -> sort $ allDepsOn graph m
  where
    opts = info (options <**> helper) ( fullDesc <> progDesc "Graph CLI tool.")
