{-# LANGUAGE RecordWildCards #-}
module Typechecker.Data.Environment where

import Prelude
import Typechecker.Data.Types
import Syntax.AbsWiadrexLang
import qualified Data.Map as M


data Env = Env
  { types :: M.Map Ident RawType
  , hasReturnStatementOccuredFlag :: Bool
  }

emptyEnv = Env 
  { types = M.fromList buildinMethodsSignatures
  , hasReturnStatementOccuredFlag = False
  }

buildinMethodsSignatures = [
  (Ident "printInt", RTFun [RTInt] RTVoid),
  (Ident "printBool", RTFun [RTBool] RTVoid),
  (Ident "printString", RTFun [RTString] RTVoid),
  (Ident "main", RTFun [] RTVoid)
  ]


updateTypes :: Env -> [(Ident, RawType)] -> Env
updateTypes env = foldl updateTupleType env

updateTupleType :: Env -> (Ident, RawType) -> Env
updateTupleType env (name, newType) = updateType env name newType

updateType :: Env -> Ident -> RawType -> Env
updateType Env{..} name newType = Env {types=M.insert name newType types, hasReturnStatementOccuredFlag=hasReturnStatementOccuredFlag}


getType :: Env -> Ident -> Maybe RawType
getType Env{..} name = M.lookup name types


returnStatementOccured :: Env -> Env
returnStatementOccured Env{..} = Env {types=types, hasReturnStatementOccuredFlag=True}

hasReturnStatementOccured :: Env -> Bool
hasReturnStatementOccured Env{..} = hasReturnStatementOccuredFlag
