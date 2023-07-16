module Interpreter.Print where

import Control.Monad.Except

import Elina.Abs

import Interpreter.TypesAndEnvironment

printFunctions :: [String]
printFunctions = ["printInt", "printBool", "printString", "printTuple"]

isPrint :: Ident -> Bool
isPrint (Ident name) = name `elem` printFunctions

evalPrint :: Value -> InterpreterMonad Value
evalPrint value = do
  liftIO $ print value
  return VVoid
