module Typechecker.Exceptions where

import Syntax.AbsWiadrexLang
import Typechecker.Types


type TypecheckerException = TypecheckerException' BNFC'Position

data TypecheckerException' a
  = UndefinedSymbolException a Ident
  | InvalidTypeException a RawType RawType
  | InvalidFunctionArgumentsTypesException a [RawType] [RawType]
  | FunctionArgumentsNameDuplicationException a [Arg]
  | InvalidReturnTypeException a RawType RawType


instance (Show a) => Show (TypecheckerException' a) where

  show (UndefinedSymbolException position name) = concat [
    "ERROR: Undefined symbol: ", show name, ", at ", show position
    ]

  show (InvalidTypeException position expectedType actualType) = concat [
    "ERROR: Invalid type! Expected: ", show expectedType, ", got: ", show actualType, ", at ", show position
   ]

  show (InvalidFunctionArgumentsTypesException position expectedTypes actualTypes) = concat [
    "ERROR: Invalid function arguments! Expected: ", show expectedTypes, ", got: ", show actualTypes, ", at ", show position
   ]

  show (FunctionArgumentsNameDuplicationException position arguments) = concat [
    "ERROR: Function arguments names duplication: ", show arguments, ", at ", show position
   ]

  show (InvalidReturnTypeException position expectedType actualType) = concat [
    "ERROR: Invalid function return type! Expected: ", show expectedType, ", got: ", show actualType, ", at ", show position
   ]

