module TypeChecker.Environment where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Map (Map, empty)
import qualified Data.Map as Map

import Elina.Abs

import TypeChecker.Errors
import TypeChecker.Types

data TypeCheckerEnv = Env {
  types         :: Map Ident TypeCheckerType,
  returnType    :: Maybe TypeCheckerType,
  returnOccured :: Bool,
  insideLoop    :: Bool
}

printFunctions :: [(Ident, TypeCheckerType)]
printFunctions = [
    (Ident "printInt", TCFn TCVoid [TCArg TCInt]),
    (Ident "printBool", TCFn TCVoid [TCArg TCBool]),
    (Ident "printString", TCFn TCVoid [TCArg TCStr]),
    (Ident "printTuple", TCFn TCVoid [TCArg (TCTuple [])])
    -- [] is a placeholder, the argument types will be checked manually
  ]

emptyTypeCheckerEnv :: TypeCheckerEnv
emptyTypeCheckerEnv = Env {
  types          = Map.fromList printFunctions,
  returnType     = Nothing,
  returnOccured  = False,
  insideLoop     = False
}

insertToEnv :: TypeCheckerEnv -> Ident -> TypeCheckerType -> TypeCheckerEnv
insertToEnv env ident value = env { types = Map.insert ident value (types env) }

insertListToEnv :: TypeCheckerEnv -> [(Ident, TypeCheckerType)] -> TypeCheckerEnv
insertListToEnv = foldr (\(ident, value) env -> insertToEnv env ident value)

setReturnType :: TypeCheckerEnv -> TypeCheckerType -> TypeCheckerEnv
setReturnType env newReturnType = env { returnType = Just newReturnType }

setReturnOccured :: TypeCheckerEnv -> Bool -> TypeCheckerEnv
setReturnOccured env newReturnOccured = env { returnOccured = newReturnOccured }

setInsideLoop :: TypeCheckerEnv -> Bool -> TypeCheckerEnv
setInsideLoop env newInsideLoop = env { insideLoop = newInsideLoop }

type TypeCheckerMonad a = ReaderT TypeCheckerEnv (Except TypeCheckerError) a
