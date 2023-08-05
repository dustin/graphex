module Graphex.Parser.GHC (module Graphex.Parser.GHC, module GHC) where

import GHC.Parser qualified as GHC
import GHC.Parser.Lexer qualified as GHC
import GHC.Parser.Errors.Types qualified as GHC
import GHC.Data.StringBuffer qualified as GHC
import GHC.Data.FastString qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import GHC.Utils.Error qualified as GHC
import GHC.Utils.Ppr qualified as GHC
import GHC.Utils.Outputable qualified as GHC
import System.IO (stdout)

-------------------------------------------------

-- | We unwrap GHC.ParseResult into something nicer to work with
data PR a = PSuccess a | PFailed PFailure

data PFailure = PFailure
  { errors :: GHC.Messages GHC.PsMessage
  , warnings :: GHC.Messages GHC.PsMessage
  }

runParser :: GHC.StringBuffer -> GHC.P a -> GHC.ParseResult a
runParser buffer parser = GHC.unP parser parseState
  where
    diagOpts = GHC.DiagOpts
      { diag_warning_flags = mempty
      , diag_fatal_warning_flags = mempty
      , diag_warn_is_error = False
      , diag_reverse_errors = False
      , diag_max_errors = Nothing
      , diag_ppr_ctx = GHC.defaultSDocContext
      }
    opts = GHC.mkParserOpts mempty diagOpts mempty False True True True
    filename = "<interactive>"
    location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
    parseState = GHC.initParserState opts buffer location

runParserString :: String -> GHC.P a -> GHC.ParseResult a
runParserString = runParser . GHC.stringToStringBuffer

runParserFile :: FilePath -> GHC.P a -> IO (PR a)
runParserFile path parser = GHC.hGetStringBuffer path >>= \buf -> case runParser buf parser of
  GHC.POk _ a -> pure $ PSuccess a
  GHC.PFailed s -> pure $ PFailed $ uncurry PFailure $ GHC.getPsMessages s

printOutputable :: GHC.Outputable a => a -> IO ()
printOutputable = GHC.printSDocLn GHC.defaultSDocContext (GHC.PageMode True) stdout . GHC.ppr

printLocated :: (GHC.Outputable a, GHC.Outputable l) => GHC.GenLocated a l -> IO ()
printLocated = GHC.printSDocLn GHC.defaultSDocContext (GHC.PageMode True) stdout . GHC.pprLocated
