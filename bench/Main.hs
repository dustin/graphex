{-# LANGUAGE RankNTypes #-}

module Main where

import Test.Tasty.Bench
import Control.Monad.Managed
import Data.Foldable
import System.IO.Temp
import Graphex.Parser
import System.IO (hClose)

main :: IO ()
main = runManaged $ do
  parseFileImportsBench <- traverse (uncurry mkFileImportBench)
    [ ("Large top-level", mkLargeTopLevel)
    , ("Large comment", mkLargeComment)
    , ("Just alotta imports", mkAlottaImports)
    ]

  liftIO $ defaultMain
    [ bgroup "parseFileImports" parseFileImportsBench
    ]

mkFileImportBench :: String -> (FilePath -> Managed ()) -> Managed Benchmark
mkFileImportBench name mk = managedBenchFile >>= tapF mk >>= pure . bench name . nfIO . parseFileImports

tapF :: Functor m => (a -> m b) -> a -> m a
tapF k x = x <$ k x

mkFile :: MonadIO m => FilePath -> [IO [String]] -> m ()
mkFile (writeFile -> out) = liftIO . traverse_ (>>= out . unlines)

managedBenchFile :: Managed FilePath
managedBenchFile = managed (withSystemTempFilePath "graphex-bench-data")

withSystemTempFilePath :: String -> (FilePath -> IO r) -> IO r
withSystemTempFilePath pat k = do
  withSystemTempFile pat $ \fp h -> do
    hClose h
    k fp

alottaImports :: [String]
alottaImports = ("import " <>) <$> do
  let a2z = ['A'..'Z']
  c1 <- a2z
  c2 <- a2z
  c3 <- a2z
  pure [c1, c2, c3]

mkLargeTopLevel :: MonadIO m => FilePath -> m ()
mkLargeTopLevel = flip mkFile
  [ pure ["module LargeTopLevel where"]
  , pure alottaImports
  , lines <$> readFile "bench/data/beegfunc"
  ]
  where

mkLargeComment :: MonadIO m => FilePath -> m ()
mkLargeComment = flip mkFile
  [ pure ["module LargeComment where"]
  , pure ["{-"]
  , lines <$> readFile "bench/data/beegfunc"
  , pure ["-}"]
  , pure alottaImports
  ]

mkAlottaImports :: MonadIO m => FilePath -> m ()
mkAlottaImports = flip mkFile
  [ pure ["module AlottaImports where"]
  , pure alottaImports
  ]

managed2 :: MonadManaged m => (forall r. (a -> b -> IO r) -> IO r) -> m (a, b)
managed2 k = managed (k . curry)
