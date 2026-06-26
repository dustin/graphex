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

unit_parseMultiLineImports :: IO ()
unit_parseMultiLineImports = do
    got <- parseFileImports "testData/parseTests/MultiLineImports.hs"
    assertEqual "" [Import "Data.Text" Nothing,
                    Import "Data.Map" Nothing,
                    Import "Data.Set" Nothing,
                    Import "Data.Maybe" Nothing,
                    Import "Data.List" Nothing] got

unit_parsePostQualified :: IO ()
unit_parsePostQualified = do
    got <- parseFileImports "testData/parseTests/PostQualified.hs"
    assertEqual "" [Import "Data.Text" Nothing,
                    Import "Data.Map.Strict" Nothing,
                    Import "Data.Set" Nothing,
                    Import "Data.Maybe" Nothing,
                    Import "Data.Map" Nothing,
                    Import "Data.List" Nothing] got

unit_parseCppImports :: IO ()
unit_parseCppImports = do
    got <- parseFileImports "testData/parseTests/CppImports.hs"
    assertEqual "" [Import "Data.Foo" Nothing,
                    Import "Data.Bar" Nothing,
                    Import "Data.Baz" Nothing] got

unit_parseMultiLineModule :: IO ()
unit_parseMultiLineModule = do
    got <- parseFileImports "testData/parseTests/MultiLineModule.hs"
    assertEqual "" [Import "Data.Text" Nothing,
                    Import "Data.Map" Nothing] got

unit_parseNoImports :: IO ()
unit_parseNoImports = do
    got <- parseFileImports "testData/parseTests/NoImports.hs"
    assertEqual "" [] got

unit_parseWhereOnOwnLine :: IO ()
unit_parseWhereOnOwnLine = do
    got <- parseFileImports "testData/parseTests/WhereOnOwnLine.hs"
    assertEqual "" [Import "Data.Text" Nothing,
                    Import "Data.Map" Nothing,
                    Import "Data.Set" Nothing] got

unit_parseEarlyTermination :: IO ()
unit_parseEarlyTermination = do
    got <- parseFileImports "testData/parseTests/EarlyTermination.hs"
    assertEqual "should stop at data declaration"
                    [Import "Data.Text" Nothing,
                    Import "Data.Map" Nothing] got

unit_parseFunctionSigTermination :: IO ()
unit_parseFunctionSigTermination = do
    got <- parseFileImports "testData/parseTests/FunctionSigTermination.hs"
    assertEqual "should stop at function signature"
                    [Import "Data.Text" Nothing,
                    Import "Data.Map" Nothing,
                    Import "Data.Set" Nothing] got

unit_parseBlockCommentEdgeCases :: IO ()
unit_parseBlockCommentEdgeCases = do
    got <- parseFileImports "testData/parseTests/BlockCommentEdgeCases.hs"
    assertEqual "should handle single-line and multi-line block comments"
                    [Import "Data.Text" Nothing,
                    Import "Data.Map" Nothing,
                    Import "Data.Set" Nothing] got

unit_parseExplicitPatternImports :: IO ()
unit_parseExplicitPatternImports = do
    got <- parseFileImports "testData/parseTests/ExplicitPatternImport.hs"
    assertEqual "should handle explicit pattern imports"
                    [Import "FirstModule" Nothing,
                    Import "SecondModule" Nothing,
                    Import "ThirdModule" Nothing,
                    Import "FourthModule" Nothing] got
