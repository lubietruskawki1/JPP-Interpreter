module TypeChecker.Errors where

import Data.List (intercalate)

import Elina.Abs

import TypeChecker.Types

data TypeCheckerError
  = BreakOrContinueOutsideLoopError BNFC'Position
  | DuplicateArgumentNamesError BNFC'Position Ident [Ident]
  | EmptyTupleError BNFC'Position Ident
  | MainHasArgumentsError BNFC'Position [TypeCheckerType]
  | MainNotVoidError BNFC'Position TypeCheckerType
  | NoMainFunctionError BNFC'Position
  | NoReturnError BNFC'Position Ident
  | ReferenceToNonVariableError BNFC'Position Ident
  | ReturnOutsideFunctionError BNFC'Position
  | UndefinedFunctionError BNFC'Position Ident
  | UndefinedVariableError BNFC'Position Ident
  | VoidVariableError BNFC'Position Ident
  | WrongArgumentNumberError BNFC'Position Ident Int Int
  | WrongArgumentTypesError BNFC'Position Ident [TypeCheckerType]
                            [TypeCheckerType]
  | WrongAssignmentError BNFC'Position Ident TypeCheckerType TypeCheckerType
  | WrongPrintTupleArgument BNFC'Position TypeCheckerType
  | WrongReturnTypeError BNFC'Position TypeCheckerType TypeCheckerType
  | WrongTupleAssignmentError BNFC'Position [TupleIdent] TypeCheckerType  
                              TypeCheckerType
  | WrongTypeError BNFC'Position TypeCheckerType TypeCheckerType
  | WrongVariableTypeError BNFC'Position Ident TypeCheckerType TypeCheckerType

instance Show TypeCheckerError where

  show (BreakOrContinueOutsideLoopError position) =
    showPosition position ++ "break or continue statement outside loop"

  show (DuplicateArgumentNamesError position ident names) =
    showPosition position ++ "duplicate argument names " ++
    show (map showIdent names) ++ " for function " ++ showIdent ident

  show (EmptyTupleError position ident) =
    showPosition position ++ "tuple " ++ showIdent ident ++
    " is empty or contains an empty tuple"

  show (MainHasArgumentsError position argTypes) =
    showPosition position ++
    "main function must not take any arguments, but takes " ++ show argTypes

  show (MainNotVoidError position returnType) =
    showPosition position ++ "Main function must return void, but returns " ++
    show returnType

  show (NoMainFunctionError position) =
    showPosition position ++ "no main function defined"

  show (NoReturnError position ident) =
    showPosition position ++ "function " ++ showIdent ident ++
    " does not return anything"

  show (ReferenceToNonVariableError position ident) =
    showPosition position ++
    "passing a non-variable by reference to function " ++ showIdent ident

  show (ReturnOutsideFunctionError position) =
    showPosition position ++ "return statement outside function"

  show (UndefinedFunctionError position ident) =
    showPosition position ++ "undefined function " ++ showIdent ident

  show (UndefinedVariableError position ident) =
    showPosition position ++ "undefined variable " ++ showIdent ident

  show (VoidVariableError position ident) =
    showPosition position ++ "variable " ++ showIdent ident ++
    " cannot be void or contain a void value"

  show (WrongArgumentNumberError position ident expected actual) =
    showPosition position ++ "wrong number of arguments for function " ++
    showIdent ident ++ " - expected " ++ show expected ++ ", but got " ++
    show actual

  show (WrongArgumentTypesError position ident expected actual) =
    showPosition position ++ "wrong type of arguments for function " ++
    showIdent ident ++ " - expected " ++ show expected ++ ", but got " ++
    show actual

  show (WrongAssignmentError position ident expected actual) =
    showPosition position ++ "wrong assignment to variable "
    ++ showIdent ident ++ " - expected " ++ show expected ++ ", but got " ++
    show actual

  show (WrongPrintTupleArgument position actual) =
    showPosition position ++
    "wrong argument for printTuple - expected tuple, but got " ++ show actual

  show (WrongReturnTypeError position expected actual) =
    showPosition position ++ "wrong return type - expected " ++ show expected ++
    ", but got " ++ show actual

  show (WrongTupleAssignmentError position idents expected actual) =
    showPosition position ++ "wrong assignment to tuple " ++
    showTupleIdents idents ++ " - expected " ++ show expected ++
    ", but got " ++ show actual

  show (WrongTypeError position expected actual) =
    showPosition position ++ "wrong type - expected " ++ show expected ++
    ", but got " ++ show actual

  show (WrongVariableTypeError position ident expected actual) =
    showPosition position ++ "wrong type of variable " ++ showIdent ident ++
    " - expected " ++ show expected ++ ", but got " ++ show actual

showIdent :: Ident -> String
showIdent (Ident ident) = ident

showTupleIdents :: [TupleIdent] -> String
showTupleIdents idents =
  "[" ++ intercalate ", " (map showTupleIdent idents) ++ "]"

showTupleIdent :: TupleIdent -> String
showTupleIdent (TupleIdent _ ident) = showIdent ident
showTupleIdent (TupleIdentNested _ idents) = showTupleIdents idents

showPosition :: BNFC'Position -> String
showPosition (Just (line, column)) =
  "typecheck error at line " ++ show line ++ ", column " ++ show column ++ ": "
showPosition Nothing = "unknown position: "
