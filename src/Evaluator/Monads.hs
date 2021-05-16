module Evaluator.Monads where

import Prelude
import Control.Monad.Except
import Control.Monad.State
import Evaluator.Data.Exceptions
import Evaluator.Data.Persistence
import Syntax.AbsWiadrexLang


type EvaluatorM = EvaluatorM' Value
type EvaluatorM' a = StateT EvaluatorPersistence (ExceptT RuntimeException IO) a


class Evaluator a where

  eval :: a -> EvaluatorM
