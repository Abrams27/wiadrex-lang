module Typechecker.Utils.Typechecker
  ( expectTypeOrThrowM
  , expectTypeM
  , expectSymbolTypeOrThrowM
  , parseReturnTypecheckResultM
  , parseTypecheckResultM
  , expectValidInitsNamesOrThrowM
  , expectValidFunctionArgumentsOrThrowM
  , assertOrThrowM
  ) where

import Prelude
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Typechecker.Data.Environment
import Typechecker.Data.Exceptions
import Typechecker.Data.Types
import Typechecker.Monads
import Syntax.AbsWiadrexLang
import qualified Typechecker.Utils.Common as CU
import qualified Typechecker.Utils.Typegetter as TGU


expectTypeOrThrowM :: Typegetter a => BNFC'Position -> Env -> RawType -> a -> TypecheckerM
expectTypeOrThrowM position env expectedType expr = do
  let typecheckResult = expectTypeM position expectedType expr env
  parseTypecheckResultM typecheckResult

expectTypeM :: Typegetter a => BNFC'Position -> RawType -> a -> Env -> Either TypecheckingException ()
expectTypeM position expectedType expr env = 
  runExcept $ runReaderT (TGU.expectTypeOrThrowM position expectedType expr) env


expectSymbolTypeOrThrowM :: BNFC'Position -> RawType -> Ident -> TypecheckerM
expectSymbolTypeOrThrowM position expectedType name = do
  env <- get
  case getType env name of
    Just rawType -> assertTypeOrThrowM expectedType rawType (InvalidTypeException position expectedType rawType)
    Nothing -> throwError $ UndefinedSymbolException position name


-- -- TODO ugly error mapping
parseReturnTypecheckResultM :: BNFC'Position -> RawType -> Either TypecheckingException () -> TypecheckerM
parseReturnTypecheckResultM _ _ (Right _) = pure ()
parseReturnTypecheckResultM position expectedType (Left exception) =
  throwError $ exception

parseTypecheckResultM :: Either TypecheckingException () -> TypecheckerM
parseTypecheckResultM (Right _) = pure ()
parseTypecheckResultM (Left exception) =
  throwError $ exception


expectValidInitsNamesOrThrowM :: BNFC'Position -> [Init] -> TypecheckerM
expectValidInitsNamesOrThrowM position inits = do
  let areNamesValid = CU.validateInitNames inits
  assertOrThrowM areNamesValid (NamesDuplicationException position)


expectValidFunctionArgumentsOrThrowM :: BNFC'Position -> [Arg] -> TypecheckerM
expectValidFunctionArgumentsOrThrowM position arguments = do
  let areArgumentsValid = CU.validateFunctionArguments arguments
  assertOrThrowM areArgumentsValid (FunctionArgumentsNameDuplicationException position arguments)


assertTypeOrThrowM :: Eq a => a -> a -> TypecheckingException -> TypecheckerM
assertTypeOrThrowM expectedType actualType exception = assertOrThrowM isValidType exception
  where
    isValidType = expectedType == actualType

assertOrThrowM :: Bool -> TypecheckingException -> TypecheckerM
assertOrThrowM True _ = pure ()
assertOrThrowM False exception = throwError exception
