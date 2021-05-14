module Typechecker.Data.Exceptions where

import Syntax.AbsWiadrexLang
import Typechecker.Data.Types


type TypecheckingException = TypecheckingException' BNFC'Position

data TypecheckingException' a
  = UndefinedSymbolException a Ident
  | InvalidTypeException a RawType RawType
  | ExpectedFunctionException a RawType
  | InvalidFunctionArgumentsTypesException a [RawType] [RawType]
  | FunctionArgumentsNameDuplicationException a [Arg]
  | InvalidReturnTypeException a RawType
  | ReturnOutOfScopeException a
  | NoReturnStatementException a
  | NamesDuplicationException a


instance (Show a) => Show (TypecheckingException' a) where

  show (UndefinedSymbolException position name) = concat [
    "ERROR: Undefined symbol: ", show name, ", at ", show position
    ]

  show (InvalidTypeException position expectedType actualType) = concat [
    "ERROR: Invalid type! Expected: ", show expectedType, ", got: ", show actualType, ", at ", show position
    ]

  show (ExpectedFunctionException position actualType) = concat [
    "ERROR: Expected function! Got: ", show actualType, ", at ", show position
    ]

  show (InvalidFunctionArgumentsTypesException position expectedTypes actualTypes) = concat [
    "ERROR: Invalid function arguments! Expected: ", show expectedTypes, ", got: ", show actualTypes, ", at ", show position
    ]

  show (FunctionArgumentsNameDuplicationException position arguments) = concat [
    "ERROR: Function arguments names duplication: ", show arguments, ", at ", show position
    ]

  show (InvalidReturnTypeException position expectedType) = concat [
    "ERROR: Invalid function return type! Expected: ", show expectedType, ", at ", show position
    ]

  show (ReturnOutOfScopeException position) = concat [
    "ERROR: Return statement out of scope, at ", show position
    ]

  show (NoReturnStatementException position) = concat [
    "ERROR: Block has no return statement at: ", show position
    ]

  show (NamesDuplicationException position) = concat [
    "ERROR: Names duplication at: ", show position
    ]