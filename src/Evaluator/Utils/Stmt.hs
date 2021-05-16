module Evaluator.Utils.Stmt where

import           Control.Monad.State
import           Evaluator.Data.Persistence
import           Evaluator.Monads
import           Prelude
import           Syntax.AbsWiadrexLang


evalVIntStmt :: (Integer -> Integer) -> Ident -> EvaluatorM
evalVIntStmt f name = do
  pers <- get
  let value = getValue name pers
  let newValue = mapVInt f value
  modify $ updateValue name newValue
  pure Dummy


evalIfNotReturnedYet :: EvaluatorM -> EvaluatorM
evalIfNotReturnedYet exec = do
  pers <- get
  if isReturnNotDefined pers then exec else pure Dummy


keepEnvAndEval :: EvaluatorM -> EvaluatorM
keepEnvAndEval exec = do
  pers <- get
  let env = getEnv pers
  exec
  modify $ putEnv env
  pure Dummy
