module Main where

import Prelude
  ( String, (++), unlines
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )



main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> interpretStdin
    f:[]       -> interpretFile f
    _          -> invalidUsage


usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse standard input."
    , "  (file)         Parse content of the file." ]
  exitFailure


interpretStdin :: IO ()
interpretStdin = do
  putStrLn "XDDD"
  exitSuccess


interpretFile :: FilePath -> IO ()
interpretFile file = do
  putStrLn "XDDD file"
  exitSuccess


invalidUsage :: IO ()
invalidUsage = do
  putStrLn "Invalid usage! Use --help."
  exitFailure
