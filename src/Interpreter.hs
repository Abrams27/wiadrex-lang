module Interpreter (interpret, interpretFile) where

import Prelude
  ( ($)
  , Either(..)
  , Int, (>)
  , String, (++), unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when )

import Syntax.AbsWiadrexLang
import Lexer.LexWiadrexLang   ( Token )
import Lexer.ParWiadrexLang   ( pProgram, myLexer )
import PrintWiadrexLang ( Print, printTree )
import SkelWiadrexLang  ()
import Typechecker.Typechecker
import Evaluator.Evaluator


type Err        = Either String


interpretFile :: FilePath -> IO ()
interpretFile file = putStrLn file >> readFile file >>= interpret

interpret :: String -> IO ()
interpret input = executeTokens parsedTokens
  where
    tokens = myLexer input
    parsedTokens = pProgram tokens

executeTokens :: Err Program -> IO ()
executeTokens (Left err) = do
  putStrLn "\nParse              Failed...\n"
  putStrLn "Tokens:"
  putStrLn err
  exitFailure
executeTokens (Right tree) =
  case checkType tree of
    Left err -> do
      putStrLn "Typechecking failed"
      putStrLn $ show err
      exitFailure
    Right _ -> do
      res <- evalProgram tree
      case res of
        Left err -> do
          putStrLn "Run failed"
          putStrLn $ show err
          exitFailure
        Right _ -> do
          putStrLn "Done\n================"
          exitSuccess
