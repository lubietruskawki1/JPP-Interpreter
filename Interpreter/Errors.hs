module Interpreter.Errors where

import Elina.Abs

data RuntimeError
  = DivisionByZeroError BNFC'Position
  | UnknownError BNFC'Position

instance Show RuntimeError where

  show (DivisionByZeroError position) =
    showPosition position ++ "division by zero"

  show (UnknownError position) =
    showPosition position ++ "unknown error"

showIdent :: Ident -> String
showIdent (Ident ident) = ident

showPosition :: BNFC'Position -> String
showPosition (Just (line, column)) =
  "runtime error at line " ++ show line ++ ", column " ++ show column ++ ": "
showPosition Nothing = "runtime error at unknown position: "
