module Interpreter (interpret, interpretFile) where

import           Control.Monad           (when)
import           Prelude                 (Either (..), FilePath, IO, Int, Show,
                                          String, getContents, mapM_, putStrLn,
                                          readFile, show, unlines, ($), (++),
                                          (>), (>>), (>>=))
import           System.Exit             (exitFailure, exitSuccess)
import           System.IO               (hPrint, stderr)

import           Evaluator.Evaluator
import           Lexer.LexWiadrexLang    (Token)
import           Lexer.ParWiadrexLang    (myLexer, pProgram)
import           Syntax.AbsWiadrexLang
import           Typechecker.Typechecker



interpretFile :: FilePath -> IO ()
interpretFile file = readFile file >>= interpret

interpret :: String -> IO ()
interpret input = typecheckAndRun parsedTokens
  where
    tokens = myLexer input
    parsedTokens = pProgram tokens

typecheckAndRun :: Either String Program -> IO ()
typecheckAndRun (Left err) = do
  hPrint stderr err
  exitFailure
typecheckAndRun (Right tree) =
  case checkType tree of
    Left err -> do
      hPrint stderr err
      exitFailure
    Right _ -> do
      executionResult <- evalProgram tree
      case executionResult of
        Left err -> do
          hPrint stderr err
          exitFailure
        Right _ -> exitSuccess
