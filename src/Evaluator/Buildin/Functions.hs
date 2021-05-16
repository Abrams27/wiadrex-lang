module Evaluator.Buildin.Functions where

import Prelude
import Evaluator.Data.Persistence
import Evaluator.Data.Exceptions
import Evaluator.Monads
import Control.Monad.State
import Control.Monad.Except
import Syntax.AbsWiadrexLang


allBuildinFunctions = ["printInt", "printBool", "printString"]

isBuildin :: Ident -> Bool
isBuildin (Ident name) = name `elem` allBuildinFunctions


evalBuildin :: Ident -> [Value] -> EvaluatorM
evalBuildin _ [value] = do
  liftIO $ putStrLn (showValue value)
  pure Dummy

evalBuildin _ _ = throwError $ UnknownRuntimeException Nothing
