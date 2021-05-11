module Typechecker.Types where

import Prelude
import Syntax.AbsWiadrexLang


data RawType
    = RTInt
    | RTString
    | RTBool
    | RTVoid
    | RTFun [RawType] RawType
  deriving (Eq)


fromType :: Type -> RawType
fromType (TInt _) = RTInt
fromType (TString _) = RTString
fromType (TBool _) = RTBool
fromType (TVoid _) = RTVoid
fromType (TFun _ argumentsTypes returnType) = RTFun rawArgumentsTypes rawReturnType
  where
    rawArgumentsTypes = map fromType argumentsTypes
    rawReturnType = fromType returnType
