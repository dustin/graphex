module ImportParserSpec where

import           Graphex.Core
import           Graphex.Parser

import           Test.Tasty
import           Test.Tasty.HUnit

unit_parseSomeFile :: IO ()
unit_parseSomeFile = do
    got <- parseFileImports "testData/parseTests/SomeFile.hs"
    assertEqual "" [Import "Data.Text" Nothing,
                    Import "SomethingElse" Nothing,
                    Import "AnotherThing" Nothing,
                    Import "Data.Maybe" Nothing,
                    Import "This.Though" Nothing,
                    Import "Data.List" (Just "base")] got
