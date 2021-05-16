module Evaluator.Utils.Expr where

import           Control.Monad.Except
import           Control.Monad.State
import           Evaluator.Buildin.Functions
import           Evaluator.Data.Exceptions
import           Evaluator.Data.Persistence
import           Evaluator.Monads
import           Prelude
import           Syntax.AbsWiadrexLang


evalVIntExpr :: Evaluator a => (Integer -> Integer -> Integer) -> a -> a -> EvaluatorM
evalVIntExpr f expr1 expr2 = do
  exprVal1 <- eval expr1
  exprVal2 <- eval expr2
  pure $ mapVInts f exprVal1 exprVal2

evalVIntToVBoolExpr :: Evaluator a => (Integer -> Integer -> Bool) -> a -> a -> EvaluatorM
evalVIntToVBoolExpr f expr1 expr2 = do
  exprVal1 <- eval expr1
  exprVal2 <- eval expr2
  pure $ mapVIntsToVBool f exprVal1 exprVal2


evalVBoolExpr :: Evaluator a => (Bool -> Bool -> Bool) -> a -> a -> EvaluatorM
evalVBoolExpr f expr1 expr2 = do
  exprVal1 <- eval expr1
  exprVal2 <- eval expr2
  pure $ mapVBools f exprVal1 exprVal2


getArgLocation :: Expr -> EvaluatorM' Location
getArgLocation (EVar _ name) = gets $ getLocation name
getArgLocation _             = pure (-1)


putArgsToPers :: (Arg, Value, Location) -> EvaluatorM
putArgsToPers (PArg _ name _, value, _) = do
  modify $ putValue name value
  pure Dummy

putArgsToPers (PArgVar position _ _, _, -1) =
  throwError $ InvalidReferenceFunctionArgumentAplicationException position

putArgsToPers (PArgVar _ name _, _, location) = do
  modify $ putLocation name location
  pure Dummy


evalBuildinAppOrRegular :: Evaluator a => Ident -> [a] -> EvaluatorM -> EvaluatorM
evalBuildinAppOrRegular ident args exec = do
  argsVal <- mapM eval args
  if isBuildin ident then evalBuildin ident argsVal else exec
