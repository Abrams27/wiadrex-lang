{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecordWildCards #-}
module Evaluator.Data.Persistence where

import Prelude
import Syntax.AbsWiadrexLang
import Data.Maybe
import qualified Data.Map as M



data Value
    = VInt Integer
    | VBool Bool
    | VString String
    | VFun [Arg] Block Env
    | VVoid
    | Dummy


mapVInt :: (Integer -> Integer) -> Value -> Value
mapVInt f (VInt val) = VInt $ f val

mapVInts :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
mapVInts f (VInt val1) (VInt val2) = VInt $ f val1 val2

mapVIntsToVBool :: (Integer -> Integer -> Bool) -> Value -> Value -> Value
mapVIntsToVBool f (VInt val1) (VInt val2) = VBool $ f val1 val2


mapVBool :: (Bool -> Bool) -> Value -> Value
mapVBool f (VBool val) = VBool $ f val

mapVBools :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
mapVBools f (VBool val1) (VBool val2) = VBool $ f val1 val2


isTrue :: Value -> Bool
isTrue (VBool True) = True
isTrue _ = False

isIntEqual :: Value -> Integer -> Bool
isIntEqual (VInt val) expectedVal = val == expectedVal
isIntEqual _ _ = False

getFunctionEnv :: Value -> Env
getFunctionEnv (VFun _ _ env) = env

getFunctionArgs :: Value -> [Arg]
getFunctionArgs (VFun args _ _) = args

getFunctionBlock :: Value -> Block
getFunctionBlock (VFun _ block _) = block

isDummy :: Value -> Bool
isDummy Dummy = True
isDummy _ = False


showValue :: Value -> String
showValue (VInt val) = show val
showValue (VBool val) = show val
showValue (VString val) = val
showValue _ = ""



data EvaluatorPersistence = EvaluatorPersistence
  { env :: Env
  , store :: Store
  }


type Location = Int


data Env = Env
  { _env :: M.Map Ident Location
  }

getEnvLocation :: Ident -> Env -> Location
getEnvLocation name Env{..} = fromJust $ M.lookup name _env

putEnvLocation :: Ident -> Location -> Env -> Env
putEnvLocation name location Env{..} =
  Env{_env=M.insert name location _env}


data Store = Store
  { _store :: M.Map Location Value
  , _lastLocation :: Int
  }

getStoreValue :: Location -> Store -> Value
getStoreValue location Store{..} = fromJust $ M.lookup location _store

updateStoreValue :: Location -> Value -> Store -> Store
updateStoreValue location value Store{..} =
  Store {_store=M.insert location value _store, _lastLocation=_lastLocation}

putStoreValue :: Value -> Store -> (Location, Store)
putStoreValue value Store{..} =
  (newLocation, Store{_store=newStore, _lastLocation=newLocation})

  where
    newLocation = _lastLocation + 1
    newStore = M.insert newLocation value _store
 

getValue :: Ident -> EvaluatorPersistence -> Value
getValue name EvaluatorPersistence{..} = getStoreValue (getEnvLocation name env) store

updateValue :: Ident -> Value -> EvaluatorPersistence -> EvaluatorPersistence
updateValue name value EvaluatorPersistence{..} =
  EvaluatorPersistence {env=env, store=updateStoreValue (getEnvLocation name env) value store}

putValue :: Ident -> Value -> EvaluatorPersistence -> EvaluatorPersistence
putValue name value EvaluatorPersistence{..} =
    EvaluatorPersistence {env=newEnv, store=newStore}

  where
    (newLocation, newStore) = putStoreValue value store
    newEnv = putEnvLocation name newLocation env


getLocation :: Ident -> EvaluatorPersistence -> Location
getLocation name EvaluatorPersistence{..} = getEnvLocation name env

putLocation :: Ident -> Location -> EvaluatorPersistence -> EvaluatorPersistence
putLocation name location EvaluatorPersistence{..} =
  EvaluatorPersistence {env=putEnvLocation name location env, store=store}


getEnv :: EvaluatorPersistence -> Env
getEnv EvaluatorPersistence{..} = env

putEnv :: Env -> EvaluatorPersistence -> EvaluatorPersistence
putEnv newEnv EvaluatorPersistence{..} =
  EvaluatorPersistence{env=newEnv, store=store}



returnLabel = Ident "return"

putReturnValue :: EvaluatorPersistence -> EvaluatorPersistence
putReturnValue = putValue returnLabel Dummy

updateReturnValue :: Value -> EvaluatorPersistence -> EvaluatorPersistence
updateReturnValue = updateValue returnLabel

getReturnValue :: EvaluatorPersistence -> Value
getReturnValue = getValue returnLabel

isReturnNotDefined :: EvaluatorPersistence -> Bool
isReturnNotDefined pers = isDummy $ getValue returnLabel pers



emptyEnv = Env 
  { _env = M.empty
  }

emptyStore = Store 
  { _store = M.empty
  , _lastLocation = 0
  }

emptyEvaluatorPersistence = EvaluatorPersistence
  { env = emptyEnv
  , store = emptyStore
  }
