module TypeChecker.Utils where

import Data.List ( (\\), nub )
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader

import Elina.Abs

import TypeChecker.Environment
import TypeChecker.Errors
import TypeChecker.Types

--------------------------
-- Common helper functions
--------------------------

getType :: BNFC'Position -> Ident -> TypeCheckerMonad TypeCheckerType
getType position ident = do
  env <- ask
  case Map.lookup ident (types env) of
    Nothing -> throwError $ UndefinedVariableError position ident
    Just varType -> return varType

-- checks if variable type is void, empty tuple or tuple with void
assertVarTypeIsCorrect :: BNFC'Position -> Ident -> TypeCheckerType ->
                          TypeCheckerMonad ()
assertVarTypeIsCorrect position ident varType = do
  if varType == TCVoid
  then throwError $ VoidVariableError position ident
  else do
    if isTuple varType
    then do
      let (TCTuple tupleTypes) = varType
      if null tupleTypes
      then throwError $ EmptyTupleError position ident
      else mapM_ (assertVarTypeIsCorrect position ident) tupleTypes
    else return ()

assertType :: BNFC'Position -> TypeCheckerType -> TypeCheckerType ->
              TypeCheckerMonad ()
assertType position expectedType actualType = do
  if actualType /= expectedType
  then throwError $ WrongTypeError position expectedType actualType
  else return ()

-------------------------------
-- Helper functions for program
-------------------------------

getMainFunctionType :: BNFC'Position -> TypeCheckerMonad TypeCheckerType
getMainFunctionType position = do
  env <- ask
  case Map.lookup (Ident "main") (types env) of
    Nothing -> throwError $ NoMainFunctionError position
    Just mainFunctionType -> do
      if not (isFunction mainFunctionType)
      then throwError $ NoMainFunctionError position
      else return mainFunctionType

---------------------------------------
-- Helper functions for top definitions
---------------------------------------

findDuplicates :: (Eq a) => [a] -> [a]
findDuplicates list = list \\ nub list

getArgName :: Arg -> Ident
getArgName (Arg _ argName _) = argName
getArgName (ArgRef _ argName _) = argName

prepareEnvForFunctionBody :: [(Ident, TypeCheckerType)] ->  TypeCheckerType ->
                             TypeCheckerMonad TypeCheckerEnv
prepareEnvForFunctionBody argList returnType = do
  env <- ask
  let envWithArguments = insertListToEnv env argList
  let envWithReturnType = setReturnType envWithArguments returnType
  return $ setInsideLoop envWithReturnType False

isVoid :: TypeCheckerType -> Bool
isVoid TCVoid = True
isVoid _ = False

----------------------------------
-- Helper functions for statements
----------------------------------

assertReturnType :: BNFC'Position -> TypeCheckerType ->
                    TypeCheckerMonad TypeCheckerEnv
assertReturnType position actualType = do
  env <- ask
  case returnType env of
    Nothing -> throwError $ ReturnOutsideFunctionError position
    Just expectedType -> do
      if actualType /= expectedType
      then throwError $ WrongReturnTypeError position expectedType actualType
      else return env

assertVarType :: BNFC'Position -> Ident -> TypeCheckerType ->
                 TypeCheckerMonad TypeCheckerEnv
assertVarType position ident expectedType = do
  actualType <- getType position ident
  if actualType /= expectedType
  then throwError $ WrongVariableTypeError position ident expectedType actualType
  else ask

assertInsideLoop :: BNFC'Position -> TypeCheckerMonad TypeCheckerEnv
assertInsideLoop position = do
  env <- ask
  if not (insideLoop env)
  then throwError $ BreakOrContinueOutsideLoopError position
  else return env

getTupleType :: TupleIdent -> TypeCheckerMonad TypeCheckerType
getTupleType (TupleIdent position ident) = do
  getType position ident
getTupleType (TupleIdentNested _ idents) = do
  TCTuple <$> mapM getTupleType idents

-----------------------------------
-- Helper functions for expressions
-----------------------------------

getFunctionType :: BNFC'Position -> Ident -> TypeCheckerMonad TypeCheckerType
getFunctionType position ident = do
  env <- ask
  case Map.lookup ident (types env) of
    Nothing -> throwError $ UndefinedFunctionError position ident
    Just functionType -> do
      if not (isFunction functionType)
      then throwError $ UndefinedFunctionError position ident
      else return functionType

isFunction :: TypeCheckerType -> Bool
isFunction (TCFn _ _) = True
isFunction _ = False

isPrintTuple :: Ident -> Bool
isPrintTuple (Ident "printTuple") = True
isPrintTuple _ = False

isTuple :: TypeCheckerType -> Bool
isTuple (TCTuple _) = True
isTuple _ = False

assertPrintTupleHasCorrectArgs :: BNFC'Position -> [TypeCheckerType] ->
                                  TypeCheckerMonad ()
assertPrintTupleHasCorrectArgs position types = do
  -- printTuple has only one argument
  if not (isTuple (head types))
  then throwError $ WrongPrintTupleArgument position (head types)
  else return ()

assertNoNonVarReference :: BNFC'Position -> Ident -> TypeCheckerArg -> Expr ->
                           TypeCheckerMonad ()
assertNoNonVarReference _ _ (TCArg _) _ = return ()
assertNoNonVarReference position ident (TCArgRef _) (EVar _ _) = return ()
assertNoNonVarReference position ident _ _ =
  throwError $ ReferenceToNonVariableError position ident

assertTupleTypeIsCorrect :: BNFC'Position -> [TypeCheckerType] ->
                            TypeCheckerMonad ()
assertTupleTypeIsCorrect position types = do
  if null types
  then throwError $ EmptyTupleError position (Ident "")
  else mapM_ (assertVarTypeIsCorrect position (Ident "")) types
