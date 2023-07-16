module TypeChecker.TypeChecker where

import qualified Data.Map as Map
import Data.List (nub, (\\))

import Control.Monad.Except
import Control.Monad.Reader

import Elina.Abs

import TypeChecker.Environment
import TypeChecker.Errors
import TypeChecker.Types
import TypeChecker.Utils

typecheck :: Program -> Either TypeCheckerError ()
typecheck program =
  runExcept $ runReaderT (typecheckProgram program) emptyTypeCheckerEnv

----------
-- Program
----------

typecheckProgram :: Program -> TypeCheckerMonad ()
typecheckProgram (Program position topDefs) = do
  env <- typecheckTopDefs topDefs

  -- check if main function exists and has correct type (void) and no arguments
  mainFunction <- local (const env) (getMainFunctionType position)
  let (TCFn returnType args) = mainFunction
  if returnType /= TCVoid
  then throwError $ MainNotVoidError position returnType
  else do
    if args /= []
    then throwError $ MainHasArgumentsError position (map getArgType args)
    else return ()

------------------
-- Top definitions
------------------

typecheckTopDefs :: [TopDef] -> TypeCheckerMonad  TypeCheckerEnv
typecheckTopDefs [] = ask
typecheckTopDefs (topDef:topDefs) = do
  env <- typecheckTopDef topDef
  local (const env) (typecheckTopDefs topDefs)

typecheckTopDef :: TopDef -> TypeCheckerMonad TypeCheckerEnv

typecheckTopDef (FnDef position ident args returnType block) = do
  let argNames = map getArgName args
  let convertedArgs = map convertArg args
  let argsList = zip argNames (map getArgType convertedArgs)

  -- check if arguments have correct types
  mapM_ (uncurry (assertVarTypeIsCorrect position)) argsList

  -- check if arguments have duplicate names
  let duplicateArgs = findDuplicates argNames
  if duplicateArgs /= []
  then throwError $ DuplicateArgumentNamesError position ident duplicateArgs
  else do
    env <- ask
    let convertedReturnType = convertType returnType
    let functionType = TCFn convertedReturnType convertedArgs
    let envWithFunction = insertToEnv env ident functionType

    {- insert arguments and return type to environment and set inside loop to
    false for typechecking function body -}
    envForBlock <- local (const envWithFunction)
                   (prepareEnvForFunctionBody argsList convertedReturnType)

    -- typecheck function body
    envAfterBlock <- local (const envForBlock) (typecheckBlock block)

    -- check if non-void function returns value
    if not (returnOccured envAfterBlock) && not (isVoid convertedReturnType)
    then throwError $ NoReturnError position ident
    else return envWithFunction

typecheckTopDef (VarDef position ident varType expr) = do
  let expectedType = convertType varType
  assertVarTypeIsCorrect position ident expectedType

  actualType <- getExprType expr
  if expectedType /= actualType
  then throwError $ WrongVariableTypeError position ident expectedType actualType
  else do
    env <- ask
    return (insertToEnv env ident expectedType)

--------
-- Block
--------

typecheckBlock :: Block -> TypeCheckerMonad TypeCheckerEnv
typecheckBlock (Block _ stmts) = do
  typecheckStmts stmts

-------------
-- Statements
-------------

typecheckStmts :: [Stmt] -> TypeCheckerMonad TypeCheckerEnv
typecheckStmts [] = ask
typecheckStmts (stmt:stmts) = do
  env <- typecheckStmt stmt
  local (const env) (typecheckStmts stmts)

typecheckStmt :: Stmt -> TypeCheckerMonad TypeCheckerEnv

typecheckStmt (Empty _) = do
  ask

typecheckStmt (BStmt _ block) = do
  typecheckBlock block

typecheckStmt (Decl _ topDef) = do
  typecheckTopDef topDef

typecheckStmt (DeclNoAss position ident varType) = do
  let expectedType = convertType varType
  assertVarTypeIsCorrect position ident expectedType
  env <- ask
  return (insertToEnv env ident expectedType)

typecheckStmt (Ass position ident expr) = do
  expectedType <- getType position ident
  actualType <- getExprType expr
  if expectedType /= actualType
  then throwError $ WrongAssignmentError position ident expectedType actualType
  else ask

typecheckStmt (Incr position ident) = do
  assertVarType position ident TCInt

typecheckStmt (Decr position ident) = do
  assertVarType position ident TCInt

typecheckStmt (Ret position expr) = do
  actualType <- getExprType expr
  assertReturnType position actualType
  env <- ask
  return (setReturnOccured env True)

typecheckStmt (VRet position) = do
  assertReturnType position TCVoid
  env <- ask
  return (setReturnOccured env True)

typecheckStmt (Cond position expr block) = do
  assertExprType position TCBool expr
  env <- ask
  local (const env) (typecheckBlock block)
  return env

typecheckStmt (CondElse position expr trueBlock falseBlock) = do
  assertExprType position TCBool expr
  env <- ask
  envTrue <- local (const env) (typecheckBlock trueBlock)
  envFalse <- local (const env) (typecheckBlock falseBlock)
  if returnOccured envTrue && returnOccured envFalse
  then return (setReturnOccured env True)
  else return env

typecheckStmt (While position expr block) = do
  assertExprType position TCBool expr
  env <- ask
  let envInsideLoop = setInsideLoop env True
  local (const envInsideLoop) (typecheckBlock block)
  return env

typecheckStmt (SExp position expr) = do
  assertExprType position TCVoid expr
  ask

typecheckStmt (Break position) = do
  assertInsideLoop position

typecheckStmt (Continue position) = do
  assertInsideLoop position

typecheckStmt (AssTuple position idents expr) = do
  expectedType <- TCTuple <$> mapM getTupleType idents
  actualType <- getExprType expr
  if expectedType /= actualType
  then throwError $ WrongTupleAssignmentError
                    position idents expectedType actualType
  else ask

--------------
-- Expressions
--------------

getExprType :: Expr -> TypeCheckerMonad TypeCheckerType

getExprType (EVar position ident) = do
  getType position ident

getExprType (ELitInt _ _) = pure TCInt

getExprType (ELitTrue _) = pure TCBool

getExprType (ELitFalse _) = pure TCBool

getExprType (EApp position ident exprs) = do
  -- check if function exists
  functionType <- getFunctionType position ident
  let (TCFn returnType args) = functionType
  -- check number of arguments
  let expectedArgsNumber = length args
  let actualArgsNumber = length exprs
  if expectedArgsNumber /= actualArgsNumber
  then throwError $ WrongArgumentNumberError
                    position ident expectedArgsNumber actualArgsNumber
  else do
    -- check types of arguments
    exprTypes <- mapM getExprType exprs
    if isPrintTuple ident
    then do
      -- check manually for printTuple
      assertPrintTupleHasCorrectArgs position exprTypes
      pure returnType
    else do
      let argTypes = map getArgType args
      if exprTypes /= argTypes
      then throwError $ WrongArgumentTypesError position ident argTypes exprTypes
      else do
        -- check if call passes non-variable arguments by reference
        let argsAndExprs = zip args exprs
        mapM_ (uncurry (assertNoNonVarReference position ident)) argsAndExprs
        pure returnType

getExprType (EString _ _) = pure TCStr

getExprType (Neg position expr) = do
  assertExprType position TCInt expr
  pure TCInt

getExprType (Not position expr) = do
  assertExprType position TCBool expr
  pure TCBool

getExprType (EMul position expr1 mulOp expr2) = do
  assertExprTypes position TCInt expr1 expr2
  pure TCInt

getExprType (EAdd position expr1 addOp expr2) = do
  assertExprTypes position TCInt expr1 expr2
  pure TCInt

getExprType (ERel position expr1 relOp expr2) = do
  assertExprTypes position TCInt expr1 expr2
  pure TCBool

getExprType (EAnd position expr1 expr2) = do
  assertExprTypes position TCBool expr1 expr2
  pure TCBool

getExprType (EOr position expr1 expr2) = do
  assertExprTypes position TCBool expr1 expr2
  pure TCBool

getExprType (ETuple position exprs) = do
  types <- mapM getExprType exprs
  assertTupleTypeIsCorrect position types
  pure $ TCTuple types

-------------------
-- Helper functions
-------------------

assertExprType :: BNFC'Position -> TypeCheckerType -> Expr ->
                  TypeCheckerMonad ()
assertExprType position expectedType expr =
  getExprType expr >>= assertType position expectedType

assertExprTypes :: BNFC'Position -> TypeCheckerType -> Expr -> Expr ->
                   TypeCheckerMonad ()
assertExprTypes position expectedType expr1 expr2 = do
  assertExprType position expectedType expr1
  assertExprType position expectedType expr2
