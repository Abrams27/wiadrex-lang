module Typechecker.Utils.Common
  ( getArgumentsWithTypes
  , validateFunctionArguments
  , validateInitNames
  , isFunctionType
  ) where

import Data.List
import Syntax.AbsWiadrexLang
import Typechecker.Data.Types


getArgumentsWithTypes :: [Arg] -> [(Ident, RawType)]
getArgumentsWithTypes = map getArgumentWithType

getArgumentWithType :: Arg -> (Ident, RawType)
getArgumentWithType (PArg _ name argType) = (name, fromType argType)
getArgumentWithType (PArgVar _ name argType) = (name, fromType argType)


validateFunctionArguments :: [Arg] -> Bool 
validateFunctionArguments arguments = numberOfArguments == numberOfUniqueArguments
  where
    argumentsNames = map getArgumentName arguments
    numberOfArguments = length argumentsNames
    numberOfUniqueArguments = length $ nub argumentsNames

validateInitNames :: [Init] -> Bool
validateInitNames inits = numberOfInits == numberOfUniqueInits
  where
    names = map getInitName inits
    numberOfInits = length names
    numberOfUniqueInits = length $ nub names

getInitName :: Init -> Ident
getInitName (IFnDef _ name _ _ _) = name
getInitName (IInit _ name _ _) = name

getArgumentName :: Arg -> Ident
getArgumentName (PArg _ name _) = name
getArgumentName (PArgVar _ name _) = name


isFunctionType :: Type -> Bool
isFunctionType (TFun _ _ _) = True
isFunctionType _ = False