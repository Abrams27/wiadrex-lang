{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Typechecker.Typechecker
  ( checkType
  ) where

import Prelude
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Typechecker.Data.Environment
import Typechecker.Data.Exceptions
import Typechecker.Data.Types
import Typechecker.Monads
import Syntax.AbsWiadrexLang
import qualified Typechecker.Utils.Typegetter as TGU
import qualified Typechecker.Utils.Typechecker as TCU
import qualified Typechecker.Utils.Common as CU


checkType :: Program -> Either TypecheckingException ()
checkType program = 
  runExcept $ evalStateT (checkTypeM Nothing program) emptyEnv


instance Typechecker Program where

  checkTypeM _ (PProgram position inits) = do
      TCU.expectValidInitsNamesOrThrowM position inits

      mapM_ updateEnvWithFunctionInitM inits
      mapM_ (checkTypeM Nothing) inits

    where
      updateEnvWithFunctionInitM :: Init -> TypecheckerM
      updateEnvWithFunctionInitM init = do
        env <- get
        put $ updateEnvWithFunctionInit env init

      updateEnvWithFunctionInit :: Env -> Init -> Env
      updateEnvWithFunctionInit env (IFnDef _ name arguments returnType _) =
        updateType env name (fromFunction arguments returnType)
      updateEnvWithFunctionInit env _ = env



instance Typechecker Init where

  checkTypeM _ (IFnDef position name arguments returnType block) = do
    TCU.expectValidFunctionArgumentsOrThrowM position arguments

    env <- get
    let functionType = fromFunction arguments returnType
    let rawReturnType = fromType returnType
    put $ updateType env name functionType

    envWithFunction <- get
    let argumentsWithTypes = CU.getArgumentsWithTypes arguments
    put $ updateTypes env argumentsWithTypes
    checkTypeM (Just rawReturnType) block
    blockEnv <- get
    TCU.assertOrThrowM (hasReturnStatementOccured blockEnv) (NoReturnStatementException position)

    put $ envWithFunction


  checkTypeM _ (IInit position name exprType expr) = do
    let rawExprType = fromType exprType
    env <- get
    TCU.expectTypeOrThrowM position env rawExprType expr

    put $ updateType env name rawExprType


instance Typechecker Block where

  checkTypeM expectedReturnType (SBlock position statements) = do
    mapM_ (checkTypeM expectedReturnType) statements


instance Typechecker Stmt where

  checkTypeM _ (SEmpty _) = pure ()

  checkTypeM expectedReturnType (SBStmt _ block) = do
    env <- get
    checkTypeM expectedReturnType block
    put env

  checkTypeM expectedReturnType (SInit _ init) =
    checkTypeM expectedReturnType init

  checkTypeM _ (SAss position name expr) = do
    env <- get
    case getType env name of
      Just rawType -> TCU.expectTypeOrThrowM position env rawType expr
      Nothing -> throwError $ UndefinedSymbolException position name

  checkTypeM _ (SIncr position name) =
    TCU.expectSymbolTypeOrThrowM position RTInt name

  checkTypeM _ (SDecr position name) =
    TCU.expectSymbolTypeOrThrowM position RTInt name

  checkTypeM (Just expectedReturnType) (SRet position returnExpr) = do
    env <- get
    let typecheckResult = TCU.expectTypeM position expectedReturnType returnExpr env
    TCU.parseReturnTypecheckResultM position expectedReturnType typecheckResult
    put $ returnStatementOccured env

  checkTypeM Nothing (SRet position _) =
    throwError $ ReturnOutOfScopeException position

  checkTypeM (Just expectedReturnType) (SRetVoid position) = do
    env <- get
    TCU.assertOrThrowM (expectedReturnType == RTVoid) (InvalidReturnTypeException position expectedReturnType)
    put $ returnStatementOccured env

  checkTypeM Nothing (SRetVoid position) =
    throwError $ ReturnOutOfScopeException position

  checkTypeM expectedReturnType (SCond position cond trueBlock) = do
    env <- get
    TCU.expectTypeOrThrowM position env RTBool cond
    checkTypeM expectedReturnType trueBlock

  checkTypeM expectedReturnType (SCondElse position cond trueBlock falseBlock) = do
    env <- get
    TCU.expectTypeOrThrowM position env RTBool cond
    checkTypeM expectedReturnType trueBlock
    checkTypeM expectedReturnType falseBlock

  checkTypeM expectedReturnType (SWhile position cond block) = do
    env <- get
    TCU.expectTypeOrThrowM position env RTBool cond
    checkTypeM expectedReturnType block

  checkTypeM _ (SExp position expr) = do
    env <- get
    TCU.expectTypeOrThrowM position env RTVoid expr



instance Typegetter Expr where

  getTypeM (ELitInt _ _) = pure RTInt

  getTypeM (ELitTrue _) = pure RTBool

  getTypeM (ELitFalse _) = pure RTBool

  getTypeM (EString _ _) = pure RTString

  getTypeM (ENeg position expr) = do
    TGU.expectTypeOrThrowM position RTInt expr
    pure RTInt

  getTypeM (ENot position expr) = do
    TGU.expectTypeOrThrowM position RTBool expr
    pure RTBool

  getTypeM (EMul position expr1 _ expr2) = do
    TGU.expectTypesOrThrowM position RTInt expr1 expr2
    pure RTInt

  getTypeM (EAdd position expr1 _ expr2) = do
    TGU.expectTypesOrThrowM position RTInt expr1 expr2
    pure RTInt

  getTypeM (ERel position expr1 _ expr2) = do
    TGU.expectTypesOrThrowM position RTInt expr1 expr2
    pure RTBool

  getTypeM (EAnd position expr1 expr2) = do
    TGU.expectTypesOrThrowM position RTBool expr1 expr2
    pure RTBool

  getTypeM (EOr position expr1 expr2) = do
    TGU.expectTypesOrThrowM position RTBool expr1 expr2
    pure RTBool

  getTypeM (EVar position name) =
    TGU.getDefinedSymbolOrThrowM position name pure

  getTypeM (EApp position name arguments) = do
    TGU.getDefinedSymbolOrThrowM position name (TGU.getFunctionTypeOrThrowM position arguments)

  getTypeM (ELambda position arguments returnType block) = do
      TGU.expectValidFunctionArgumentsOrThrowM position arguments
      let argumentsWithTypes = CU.getArgumentsWithTypes arguments

      env <- ask
      local (\env -> updateTypes env argumentsWithTypes) (runLocalCheckM arguments returnType block)

    where
      runLocalCheckM :: Typechecker a => [Arg] -> Type -> a -> TypegetterM
      runLocalCheckM arguments returnType block = do
        let rawReturnType = fromType returnType
        let functionType = fromFunction arguments returnType
        
        env <- ask
        TGU.checkTypeOrThrowM env (Just rawReturnType) block checkLambdaBodyM
        pure functionType

      checkLambdaBodyM :: Typechecker a => Maybe RawType -> a -> TypecheckerM
      checkLambdaBodyM expectedType block = do
        checkTypeM expectedType block
        blockEnv <- get
        TCU.assertOrThrowM (hasReturnStatementOccured blockEnv) (NoReturnStatementException position)

