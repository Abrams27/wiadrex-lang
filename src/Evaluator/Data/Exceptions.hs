module Evaluator.Data.Exceptions where

import Syntax.AbsWiadrexLang


type RuntimeException = RuntimeException' BNFC'Position

data RuntimeException' a
  = UnknownRuntimeException a
  | InvalidReferenceFunctionArgumentAplicationException a
  | DivideByZeroException a


instance (Show a) => Show (RuntimeException' a) where

  show (UnknownRuntimeException position) = concat [
    "RUNTIME EXCEPTION: Unknown exception! At: ", show position
    ]

  show (InvalidReferenceFunctionArgumentAplicationException position) = concat [
    "RUNTIME EXCEPTION: Invalid reference function argument! Argument has to be variable! At: ", show position
    ]

  show (DivideByZeroException position) = concat [
    "RUNTIME EXCEPTION: Divide by zero! At: ", show position
    ]