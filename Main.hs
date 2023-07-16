module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, hPrint)

import Elina.Par (myLexer, pProgram)

import TypeChecker.TypeChecker (typecheck)
import Interpreter.Interpreter (run)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [file]     -> interpretFile file
    []         -> interpretStdin
    _          -> invalidUsage

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "Usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    ]
  exitFailure

interpretFile :: FilePath -> IO ()
interpretFile file = readFile file >>= interpret

interpretStdin :: IO ()
interpretStdin = getContents >>= interpret

invalidUsage :: IO ()
invalidUsage = do
  putStrLn "Invalid usage. Try --help."
  exitFailure

interpret :: String -> IO ()
interpret input = do
  let tokens = myLexer input
  case pProgram tokens of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right tree -> do
      case typecheck tree of
        Left err -> do
          hPrint stderr err
          exitFailure
        Right _ -> do
          result <- run tree
          case result of
            Left err -> do
              hPrint stderr err
              exitFailure
            Right _ -> do
              exitSuccess
