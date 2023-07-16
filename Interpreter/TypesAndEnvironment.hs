module Interpreter.TypesAndEnvironment where

import Data.Map (Map, empty)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Elina.Abs

import Interpreter.Errors

data Value
  = VInt Integer
  | VStr String
  | VBool Bool
  | VVoid
  | VTuple [Value]
  | VFn [Arg] Block Env

instance Show Value where
  show (VInt i) = show i
  show (VStr s) = s
  show (VBool True) = "true"
  show (VBool False) = "false"
  show (VTuple values) =
    "[" ++ intercalate ", " (map showValueInTuple values) ++ "]"
  show _ = ""

showValueInTuple :: Value -> String
showValueInTuple (VStr s) = show s
showValueInTuple value = show value

type Location = Int

newtype Env = Env {
  locations :: Map Ident Location
}

emptyEnv :: Env
emptyEnv = Env {
  locations = Map.empty
}

data Store = Store {
  values       :: Map Location Value,
  nextLocation :: Location
}

emptyStore :: Store
emptyStore = Store {
  values       = Data.Map.empty,
  nextLocation = 0
}

getNextLocation :: Store -> (Store, Location)
getNextLocation store =
  (store { nextLocation = nextLocation store + 1 }, nextLocation store)

insertValueToStore :: Store -> Value -> Store
insertValueToStore store value =
  store { values = Map.insert (nextLocation store) value (values store),
          nextLocation = nextLocation store + 1 }

insertValueToEnv :: Env -> Ident -> Location -> Env
insertValueToEnv env ident location =
  env { locations = Map.insert ident location (locations env) }

insertValueToStoreAndEnv :: Store -> Env -> Ident -> Value -> (Store, Env)
insertValueToStoreAndEnv oldStore oldEnv ident value =
  let location = nextLocation oldStore
      newStore = insertValueToStore oldStore value
      newEnv = insertValueToEnv oldEnv ident location
  in (newStore, newEnv)

updateValueInStore :: Store -> Location -> Value -> Store
updateValueInStore store location value =
  store { values = Map.insert location value (values store) }

getLocationFromEnv :: Env -> Ident -> Location
getLocationFromEnv env name = fromJust $ Map.lookup name (locations env)

getValueFromStore :: Store -> Location -> Value
getValueFromStore store location = fromJust $ Map.lookup location (values store)

type InterpreterMonad a = ReaderT Env (StateT Store (ExceptT RuntimeError IO)) a

data Result = RNothing | RBreak | RContinue | RValue Value
