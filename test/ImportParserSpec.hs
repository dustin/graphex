module ImportParserSpec where

import Graphex.Parser
import Graphex.Core

import           Test.Tasty
import           Test.Tasty.HUnit

unit_parseSomeFile :: IO ()
unit_parseSomeFile = do
    got <- parseFileImports "testData/parseTests/SomeFile.hs"
    assertEqual "" ["Data.Text", "SomethingElse", "AnotherThing", "Data.Maybe", "This.Though"] (module_ <$> got)