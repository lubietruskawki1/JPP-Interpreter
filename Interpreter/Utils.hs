module Interpreter.Utils where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Elina.Abs

import Interpreter.Errors
import Interpreter.TypesAndEnvironment

--------------------------
-- Common helper functions
--------------------------

returnUnchangedEnvAndNothing :: InterpreterMonad (Env, Result)
returnUnchangedEnvAndNothing = liftM2 (,) ask (return RNothing)

insertValue :: Ident -> Value -> InterpreterMonad Env
insertValue ident value = do
  env <- ask
  store <- get
  let (newStore, newEnv) = insertValueToStoreAndEnv store env ident value
  put newStore
  return newEnv

getValue :: Ident -> InterpreterMonad Value
getValue ident = do
  env <- ask
  store <- get
  return $ getValueFromStore store (getLocationFromEnv env ident)

updateValue :: Ident -> Value -> InterpreterMonad (Env, Result)
updateValue ident value = do
  env <- ask
  store <- get
  let newStore = updateValueInStore store (getLocationFromEnv env ident) value
  put newStore
  returnUnchangedEnvAndNothing

----------------------------------
-- Helper functions for statements
----------------------------------

isNothing :: Result -> Bool
isNothing RNothing = True
isNothing _ = False

isBreak :: Result -> Bool
isBreak RBreak = True
isBreak _ = False

getDefaultValue :: BNFC'Position -> Type -> InterpreterMonad Value
getDefaultValue _ (Int _) = pure $ VInt 0
getDefaultValue _ (Str _) = pure $ VStr ""
getDefaultValue _ (Bool _) = pure $ VBool False
getDefaultValue position (Tuple _ idents) = do
  let values = map (getDefaultValue position) idents
  VTuple <$> sequence values
getDefaultValue position _ = throwError $ UnknownError position

evalModification :: (Integer -> Integer) -> Ident ->
                    InterpreterMonad (Env, Result)
evalModification op ident = do
  (VInt value) <- getValue ident
  let newValue = VInt (op value)
  updateValue ident newValue

isTrue :: Value -> Bool
isTrue (VBool True) = True
isTrue _ = False

updateTupleIdent :: BNFC'Position -> TupleIdent -> Value ->
                    InterpreterMonad (Env, Result)
updateTupleIdent _ (TupleIdent _ ident) value = do
  updateValue ident value
updateTupleIdent position (TupleIdentNested _ idents) (VTuple values) = do
  updateTuple position idents values
updateTupleIdent position _ _ = throwError $ UnknownError position

updateTuple :: BNFC'Position -> [TupleIdent] -> [Value] ->
               InterpreterMonad (Env, Result)
updateTuple _ [] [] = returnUnchangedEnvAndNothing
updateTuple position (ident:idents) (value:values) = do
  (env, _) <- updateTupleIdent position ident value
  local (const env) (updateTuple position idents values)
updateTuple position _ _ = throwError $ UnknownError position

-----------------------------------
-- Helper functions for expressions
-----------------------------------

getArgLocation :: Env -> Expr -> Location
getArgLocation env (EVar _ ident) = getLocationFromEnv env ident
getArgLocation _ _ = -1

insertArgsToEnv :: BNFC'Position -> [Arg] -> [Value] -> [Location] -> Ident ->
                   InterpreterMonad Env
insertArgsToEnv _ [] [] [] _ = ask
insertArgsToEnv position (arg:args) (value:values)
                (location:locations) functionIdent = do
  env <- insertArgToEnv arg value location functionIdent
  local (const env)
    (insertArgsToEnv position args values locations functionIdent)
insertArgsToEnv position _ _ _ _ = throwError $ UnknownError position

insertArgToEnv :: Arg -> Value -> Location -> Ident -> InterpreterMonad Env

insertArgToEnv (Arg position ident _) value location functionIdent = do
  insertValue ident value
  
insertArgToEnv (ArgRef position ident _) value location functionIdent = do
  if location == -1
  then throwError $ UnknownError position
  else do
    env <- ask
    return $ insertValueToEnv env ident location

isZero :: Value -> Bool
isZero (VInt 0) = True
isZero _ = False
