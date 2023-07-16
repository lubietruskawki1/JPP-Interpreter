module TypeChecker.Types where

import Data.List (intercalate)

import Elina.Abs

data TypeCheckerType
  = TCInt
  | TCStr
  | TCBool
  | TCVoid
  | TCTuple [TypeCheckerType]
  | TCFn TypeCheckerType [TypeCheckerArg]
  deriving (Eq)

data TypeCheckerArg
  = TCArg TypeCheckerType
  | TCArgRef TypeCheckerType
  deriving (Eq)

instance Show TypeCheckerType where
  show TCInt = "int"
  show TCStr = "string"
  show TCBool = "bool"
  show TCVoid = "void"
  show (TCTuple types) = "tuple <" ++ intercalate "," (map show types) ++ ">"
  show (TCFn returnType argTypes) =
    show "fn (" ++ show argTypes ++ ") -> " ++ show returnType

instance Show TypeCheckerArg where
  show (TCArg argType) = show argType
  show (TCArgRef argType) = show "ref " ++ show argType

convertType :: Type -> TypeCheckerType
convertType (Int _) = TCInt
convertType (Str _) = TCStr
convertType (Bool _) = TCBool
convertType (Void _) = TCVoid
convertType (Tuple _ types) = TCTuple (map convertType types)

convertArg :: Arg -> TypeCheckerArg
convertArg (Arg _ _ argType) = TCArg (convertType argType)
convertArg (ArgRef _ _ argType) = TCArgRef (convertType argType)

getArgType :: TypeCheckerArg -> TypeCheckerType
getArgType (TCArg argType) = argType
getArgType (TCArgRef argType) = argType
