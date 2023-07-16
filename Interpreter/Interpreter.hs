module Interpreter.Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Elina.Abs

import Interpreter.Errors
import Interpreter.Print
import Interpreter.TypesAndEnvironment
import Interpreter.Utils

run :: Program -> IO (Either RuntimeError Value)
run program = do
  runExceptT (evalStateT (runReaderT (evalProgram program) emptyEnv) emptyStore)

----------
-- Program
----------

evalProgram :: Program -> InterpreterMonad Value
evalProgram (Program position topDefs) = do
  (env, _) <- evalTopDefs topDefs
  local (const env) (evalExpr (EApp position (Ident "main") []))

------------------
-- Top definitions
------------------

evalTopDefs :: [TopDef] -> InterpreterMonad (Env, Result)
evalTopDefs [] = returnUnchangedEnvAndNothing
evalTopDefs (topDef:topDefs) = do
  (env, _) <- evalTopDef topDef
  local (const env) (evalTopDefs topDefs)

evalTopDef :: TopDef -> InterpreterMonad (Env, Result)

evalTopDef (FnDef _ ident args returnType block) = do
  env <- ask
  let function = VFn args block env
  envWithFunction <- insertValue ident function
  return (envWithFunction, RNothing)

evalTopDef (VarDef _ ident varType expr) = do
  value <- evalExpr expr
  envWithVariable <- insertValue ident value
  return (envWithVariable, RNothing)

--------
-- Block
--------

evalBlock :: Block -> InterpreterMonad (Env, Result)
evalBlock (Block _ stmts) = do
  env <- ask
  local (const env) (evalStmts stmts)

-------------
-- Statements
-------------

evalStmts :: [Stmt] -> InterpreterMonad (Env, Result)
evalStmts [] = returnUnchangedEnvAndNothing
evalStmts (stmt:stmts) = do
  (env, value) <- evalStmt stmt
  -- check if return, break or continue statement occurred
  if isNothing value
  then local (const env) (evalStmts stmts)
  else return (env, value)

evalStmt :: Stmt -> InterpreterMonad (Env, Result)

evalStmt (Empty _) = do
  returnUnchangedEnvAndNothing

evalStmt (BStmt _ block) =  do
  env <- ask
  (_, value) <- local (const env) (evalBlock block)
  return (env, value)

evalStmt (Decl _ topDef) = do
  evalTopDef topDef

evalStmt (DeclNoAss position ident varType) = do
  value <- getDefaultValue position varType
  envWithVariable <- insertValue ident value
  return (envWithVariable, RNothing)

evalStmt (Ass _ ident expr) = do
  value <- evalExpr expr
  updateValue ident value

evalStmt (Incr _ ident) = do
  evalModification (+ 1) ident

evalStmt (Decr _ ident) = do
  evalModification (\x -> x - 1) ident

evalStmt (Ret _ expr) = do
  env <- ask
  value <- evalExpr expr
  return (env, RValue value)

evalStmt (VRet _) = do
  env <- ask
  return (env, RValue VVoid)

evalStmt (Cond _ expr block) = do
  env <- ask
  condition <- evalExpr expr
  if isTrue condition
  then do
    (_, value) <- local (const env) (evalBlock block)
    return (env, value)
  else returnUnchangedEnvAndNothing

evalStmt (CondElse _ expr trueBlock falseBlock) = do
  env <- ask
  condition <- evalExpr expr
  if isTrue condition
  then do
    (_, value) <- local (const env) (evalBlock trueBlock)
    return (env, value)
  else do
    (_, value) <- local (const env) (evalBlock falseBlock)
    return (env, value)

evalStmt (While position expr block) = do
  env <- ask
  condition <- evalExpr expr
  if isTrue condition
  then do
    (_, value) <- local (const env) (evalBlock block)
    -- check if return or break statement occurred
    case value of
      RContinue -> continueLoop env
      RNothing  -> continueLoop env
      RBreak    -> return (env, RNothing)
      _         -> return (env, value)
  else returnUnchangedEnvAndNothing
  where
    continueLoop :: Env -> InterpreterMonad (Env, Result)
    continueLoop env = local (const env) (evalStmt (While position expr block))

evalStmt (SExp _ expr) = do
  value <- evalExpr expr
  returnUnchangedEnvAndNothing

evalStmt (Break _) = do
  env <- ask
  return (env, RBreak)

evalStmt (Continue _) = do
  env <- ask
  return (env, RContinue)

evalStmt (AssTuple position idents expr) = do
  (VTuple values) <- evalExpr expr
  updateTuple position idents values

--------------
-- Expressions
--------------

evalExpr :: Expr -> InterpreterMonad Value

evalExpr (EVar _ ident) = do
  getValue ident

evalExpr (ELitInt _ integer) = pure $ VInt integer

evalExpr (ELitTrue _) = pure $ VBool True

evalExpr (ELitFalse _) = pure $ VBool False

evalExpr (EApp position ident exprs) = do
  if isPrint ident
  then do
    -- print has only one argument
    value <- evalExpr (head exprs)
    evalPrint value
  else do
    env <- ask
    argValues <- mapM evalExpr exprs
    let argLocations = map (getArgLocation env) exprs
    function@(VFn args block functionEnv) <- getValue ident

    -- insert function to environment to allow recursion
    envWithFunction <- local (const functionEnv) (insertValue ident function)

    -- insert arguments to environment
    envWithArgs <- local (const envWithFunction)
                   (insertArgsToEnv position args argValues argLocations ident)

    -- evaluate function body
    (_, result) <- local (const envWithArgs) (evalBlock block)

    case result of
      RValue value -> pure value
      _            -> pure VVoid

evalExpr (EString _ string) = pure $ VStr string

evalExpr (Neg _ expr) = do
  (VInt value) <- evalExpr expr
  pure $ VInt (-value)

evalExpr (Not _ expr) = do
  (VBool value) <- evalExpr expr
  pure $ VBool (not value)

evalExpr (EMul _ expr1 (Times _) expr2) = do
  evalOperation (*) expr1 expr2

evalExpr (EMul position expr1 (Div _) expr2) = do
  value2 <- evalExpr expr2
  if isZero value2
  then throwError $ DivisionByZeroError position
  else evalOperation div expr1 expr2

evalExpr (EMul _ expr1 (Mod _) expr2) = do
  evalOperation mod expr1 expr2

evalExpr (EAdd _ expr1 (Plus _) expr2) = do
  evalOperation (+) expr1 expr2

evalExpr (EAdd _ expr1 (Minus _) expr2) = do
  evalOperation (-) expr1 expr2

evalExpr (ERel _ expr1 (LTH _) expr2) = do
  evalComparison (<) expr1 expr2

evalExpr (ERel _ expr1 (LE _) expr2) = do
  evalComparison (<=) expr1 expr2

evalExpr (ERel _ expr1 (GTH _) expr2) = do
  evalComparison (>) expr1 expr2

evalExpr (ERel _ expr1 (GE _) expr2) = do
  evalComparison (>=) expr1 expr2

evalExpr (ERel _ expr1 (EQU _) expr2) = do
  evalComparison (==) expr1 expr2

evalExpr (ERel _ expr1 (NE _) expr2) = do
  evalComparison (/=) expr1 expr2

evalExpr (EAnd _ expr1 expr2) = do
  evalLogicalOperation (&&) expr1 expr2

evalExpr (EOr _ expr1 expr2) = do
  evalLogicalOperation (||) expr1 expr2

evalExpr (ETuple position exprs) = do
  VTuple <$> mapM evalExpr exprs

-------------------
-- Helper functions
-------------------

evalOperation :: (Integer -> Integer -> Integer) -> Expr -> Expr ->
                 InterpreterMonad Value
evalOperation op expr1 expr2 = do
  (VInt value1) <- evalExpr expr1
  (VInt value2) <- evalExpr expr2
  pure $ VInt (op value1 value2)

evalComparison :: (Integer -> Integer -> Bool) -> Expr -> Expr ->
                  InterpreterMonad Value
evalComparison op expr1 expr2 = do
  (VInt value1) <- evalExpr expr1
  (VInt value2) <- evalExpr expr2
  pure $ VBool (op value1 value2)

evalLogicalOperation :: (Bool -> Bool -> Bool) -> Expr -> Expr ->
                        InterpreterMonad Value
evalLogicalOperation op expr1 expr2 = do
  (VBool value1) <- evalExpr expr1
  (VBool value2) <- evalExpr expr2
  pure $ VBool (op value1 value2)
