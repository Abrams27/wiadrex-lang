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


instance Show RawType where

  show RTInt = "Int"

  show RTString = "String"

  show RTBool = "Bool"

  show RTVoid = "Void"

  show (RTFun argsTypes returnType) = concat [
    "(", show argsTypes, ")", " -> ", show returnType
    ]


fromType :: Type -> RawType
fromType (TInt _) = RTInt
fromType (TString _) = RTString
fromType (TBool _) = RTBool
fromType (TVoid _) = RTVoid
fromType (TFun _ argumentsTypes returnType) = RTFun rawArgumentsTypes rawReturnType
  where
    rawArgumentsTypes = map fromType argumentsTypes
    rawReturnType = fromType returnType
