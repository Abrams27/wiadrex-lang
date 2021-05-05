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

import AbsWiadrexLang   ()
import LexWiadrexLang   ( Token )
import ParWiadrexLang   ( pProgram, myLexer )
import PrintWiadrexLang ( Print, printTree )
import SkelWiadrexLang  ()


type Err        = Either String
type ParseFun a = [Token] -> Err a


-- TODO: type update
interpretFile :: FilePath -> IO ()
interpretFile file = putStrLn file >> readFile file >>= interpret

-- TODO: type update
interpret :: String -> IO ()
interpret input = parseOut parsedTokens
  where
    tokens = myLexer input
    parsedTokens = pProgram tokens

-- TODO: change name
parseOut :: (Show a, Print a) => Err a -> IO ()
parseOut (Left err) = do
  putStrLn "\nParse              Failed...\n"
  putStrLn "Tokens:"
  putStrLn err
  exitFailure
parseOut (Right tree) = do
  putStrLn "\nParse Successful!"
  showTree tree
  exitSuccess


showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
  putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree
