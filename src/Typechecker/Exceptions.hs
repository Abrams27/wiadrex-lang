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

-- instance Show TypecheckerException where
--   show