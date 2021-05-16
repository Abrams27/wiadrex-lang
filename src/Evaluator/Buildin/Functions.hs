module Evaluator.Buildin.Functions where

import           Control.Monad.Except
import           Control.Monad.State
import           Evaluator.Data.Exceptions
import           Evaluator.Data.Persistence
import           Evaluator.Monads
import           Prelude
import           Syntax.AbsWiadrexLang


allBuildinFunctions = ["printInt", "printBool", "printString"]

isBuildin :: Ident -> Bool
isBuildin (Ident name) = name `elem` allBuildinFunctions


evalBuildin :: Ident -> [Value] -> EvaluatorM
evalBuildin _ [value] = do
  liftIO $ putStrLn (showValue value)
  pure Dummy

evalBuildin _ _ = throwError $ UnknownRuntimeException Nothing
