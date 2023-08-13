{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphex.Parser where

import Data.Text qualified as T
import Graphex.Core

import GHC.Parser qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import GHC.Hs qualified as GHC

import Graphex.Parser.GHC qualified as GHC

import Data.Void
import Data.Functor (($>))
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.String (IsString)
import Control.Applicative (asum)
import Data.Maybe (isJust)

importParser
  :: MonadParsec e s m
  => Token s ~ Char
  => IsString (Tokens s)
  => m Import
importParser = do
  _ <- newline
  _ <- string "import"
  space1
  isQualified <- isJust <$> optional (string "qualified")
  space
  pkg <- optional $ between (char '"') (char '"') $ some $ alphaNumChar <|> char '-' <|> char '_'
  space

  modid <- some $ alphaNumChar <|> char '.' <|> char '_'

  pure Import
    { module_ = ModuleName $ T.pack modid
    , package = T.pack <$> pkg
    }

importsParser
  :: MonadParsec e s m
  => Token s ~ Char
  => IsString (Tokens s)
  => m [Import]
importsParser = go []
  where
    go acc = do
      done <- isJust <$> optional eof
      if done then pure acc else
        asum [Just <$> try importParser, anySingle $> Nothing] >>= \case
          Nothing -> go acc
          Just i -> go (i : acc)
  
parseFileImports :: FilePath -> IO [Import]
parseFileImports fp = do
  contents <- readFile fp
  case runParser (importsParser @Void) fp contents of
    Left err -> error $ unlines $ [mconcat ["Failed to parse ", fp, ":"], errorBundlePretty err]
    Right x -> pure x
