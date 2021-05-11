module Typechecker.Typechecker where

import Prelude
import Typechecker.Exceptions
import Typechecker.Types
import Syntax.AbsWiadrexLang
import Control.Monad.Reader
import Control.Monad.Except
import Data.List
import qualified Data.Map as M


type TypecheckerM = TypecheckerM' RawType
type TypecheckerM' a = ReaderT Env (Except TypecheckerException) a
type Env = M.Map Ident RawType


getTypeM :: Expr -> TypecheckerM

-- literals
getTypeM (ELitInt _ _) = pure $ RTInt

getTypeM (ELitTrue _) = pure $ RTBool

getTypeM (ELitFalse _) = pure $ RTBool

getTypeM (EString _ _) = pure $ RTString

-- operators
getTypeM (ENeg position expr) = do
  expectTypeOrThrowM position RTInt expr
  pure $ RTInt

getTypeM (ENot position expr) = do
  expectTypeOrThrowM position RTBool expr
  pure $ RTBool

getTypeM (EMul position expr1 _ expr2) = do
  expectTypesOrThrowM position RTInt expr1 expr2
  pure $ RTInt

getTypeM (EAdd position expr1 _ expr2) = do
  expectTypesOrThrowM position RTInt expr1 expr2
  pure $ RTInt

getTypeM (ERel position expr1 _ expr2) = do
  expectTypesOrThrowM position RTBool expr1 expr2
  pure $ RTBool

getTypeM (EAnd position expr1 expr2) = do
  expectTypesOrThrowM position RTBool expr1 expr2
  pure $ RTBool

getTypeM (EOr position expr1 expr2) = do
  expectTypesOrThrowM position RTBool expr1 expr2
  pure $ RTBool

-- complex
getTypeM (EVar position name) = do
  env <- ask
  case M.lookup name env of
    Just t -> pure t
    Nothing -> throwError $ UndefinedSymbolException position name


getTypeM (EApp position name arguments) = do
    env <- ask
    case M.lookup name env of
      Just rawType -> getFunctionTypeOrThrowM position rawType arguments
      Nothing -> throwError $ UndefinedSymbolException position name
  where
    getFunctionTypeOrThrowM :: BNFC'Position -> RawType -> [Expr] -> TypecheckerM
    getFunctionTypeOrThrowM position (RTFun argTypes returnType) actualArgs = do
      actualArgsTypes <- mapM getTypeM actualArgs
      expectFunctionArgumentsOrThrowM position argTypes actualArgsTypes
      pure returnType

    expectFunctionArgumentsOrThrowM :: BNFC'Position -> [RawType] -> [RawType] -> TypecheckerM' ()
    expectFunctionArgumentsOrThrowM position expectedArguments actualArguments = assertOrThrow isValidType exception
      where
        isValidType = expectedArguments == actualArguments
        exception = InvalidFunctionArgumentsTypesException position expectedArguments actualArguments


getTypeM (ELambda position arguments returnType block) = do
    let argumentsNames = map getArgumentName arguments
    let numberOfArguments = length argumentsNames
    let numberOfUniqueArguments = length $ nub argumentsNames
    assertOrThrow (numberOfArguments == numberOfUniqueArguments) (FunctionArgumentsNameDuplicationException position arguments)

    let rawArgumentsTypes = map getArgumentType arguments
    let rawReturnType = fromType returnType

    pure rawReturnType

  where
    getArgumentName :: Arg -> Ident
    getArgumentName (PArg _ name _) = name
    getArgumentName (PArgVar _ name _) = name

    getArgumentType :: Arg -> RawType
    getArgumentType (PArg _ _ argType) = fromType argType
    getArgumentType (PArgVar _ _ argType) = fromType argType






expectTypesOrThrowM :: BNFC'Position -> RawType -> Expr -> Expr -> TypecheckerM' ()
expectTypesOrThrowM position expectedType expr1 expr2 = do
  expectTypeOrThrowM position expectedType expr1
  expectTypeOrThrowM position expectedType expr2

expectTypeOrThrowM :: BNFC'Position -> RawType -> Expr -> TypecheckerM' ()
expectTypeOrThrowM position expectedType expr = do
    exprType <- getTypeM expr
    assertTypeOrThrow position expectedType exprType

assertTypeOrThrow :: BNFC'Position -> RawType -> RawType -> TypecheckerM' ()
assertTypeOrThrow position expectedType actualType = assertOrThrow isValidType exception
  where
    isValidType = expectedType == actualType
    exception = InvalidTypeException position expectedType actualType

assertOrThrow :: Bool -> TypecheckerException -> TypecheckerM' ()
assertOrThrow True _ = pure ()
assertOrThrow False exception = throwError exception
