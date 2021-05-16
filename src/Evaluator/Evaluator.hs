{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Evaluator.Evaluator 
  ( evalProgram
  ) where

import Prelude
import Evaluator.Data.Persistence
import Evaluator.Data.Exceptions
import Evaluator.Monads
import Control.Monad.Except
import Control.Monad.State
import Syntax.AbsWiadrexLang
import qualified Evaluator.Utils.Expr as ExprU
import qualified Evaluator.Utils.Stmt as StmtU


evalProgram :: Program -> IO (Either RuntimeException Value)
evalProgram program =
  runExceptT $ evalStateT (eval program) emptyEvaluatorPersistence



instance Evaluator Program where

  eval (PProgram position inits) = do
    mapM_ eval inits
    eval (EApp position (Ident "main") [])



instance Evaluator Init where

  eval (IFnDef _ name arguments _ block) = do
    pers <- get
    let funType = VFun arguments block (getEnv pers)
    modify $ putValue name funType
    pure Dummy

  eval (IInit _ name _ expr) = do
    exprVal <- eval expr
    modify $ putValue name exprVal
    pure Dummy



instance Evaluator Block where

  eval (SBlock _ statements) = do
    mapM_ eval statements
    pure Dummy



instance Evaluator Stmt where

  eval (SEmpty _) = StmtU.evalIfNotReturnedYet $
    pure Dummy


  eval (SBStmt _ block) = StmtU.evalIfNotReturnedYet $
    StmtU.keepEnvAndEval $ eval block

  eval (SInit _ init) = StmtU.evalIfNotReturnedYet $
    eval init

  eval (SAss _ name expr) = StmtU.evalIfNotReturnedYet $ do
    exprVal <- eval expr
    modify $ updateValue name exprVal
    pure Dummy


  eval (SIncr _ name) = StmtU.evalIfNotReturnedYet $
    StmtU.evalVIntStmt (1 +) name

  eval (SDecr _ name) = StmtU.evalIfNotReturnedYet $
    StmtU.evalVIntStmt (\x -> x - 1) name


  eval (SRet _ expr) = StmtU.evalIfNotReturnedYet $ do
    exprVal <- eval expr
    modify $ updateReturnValue exprVal
    pure Dummy

  eval (SRetVoid _) = StmtU.evalIfNotReturnedYet $ do
    modify $ updateReturnValue VVoid
    pure Dummy


  eval (SCond _ cond block) = StmtU.evalIfNotReturnedYet $ do
    condValue <- eval cond
    StmtU.keepEnvAndEval $
      if isTrue condValue then eval block else pure Dummy

  eval (SCondElse _ cond trueBlock falseBlock) = StmtU.evalIfNotReturnedYet $ do
    condValue <- eval cond
    StmtU.keepEnvAndEval $
      if isTrue condValue then eval trueBlock else eval falseBlock

  eval while@(SWhile _ cond block) = StmtU.evalIfNotReturnedYet $ do
    condValue <- eval cond
    StmtU.keepEnvAndEval $
      if isTrue condValue then eval block >> eval while else pure Dummy


  eval (SExp _ expr) = StmtU.evalIfNotReturnedYet $
    eval expr



instance Evaluator Expr where

  eval (ELitInt _ value) = pure $ VInt value

  eval (ELitTrue _) = pure $ VBool True

  eval (ELitFalse _) = pure $ VBool False

  eval (EString _ value) = pure $ VString value


  eval (ENeg _ expr) = do
    exprVal <- eval expr
    pure $ mapVInt ((-1) *) exprVal

  eval (ENot _ expr) = do
    exprVal <- eval expr
    pure $ mapVBool not exprVal


  eval (EMul _ expr1 (OTimes _) expr2) = ExprU.evalVIntExpr (*) expr1 expr2

  eval (EMul position expr1 (ODiv _) expr2) = do
    expr2Val <- eval expr2
    if isIntEqual expr2Val 0 then throwError $ DivideByZeroException position else pure Dummy
    ExprU.evalVIntExpr quot expr1 expr2

  eval (EMul _ expr1 (OMod _) expr2) = ExprU.evalVIntExpr mod expr1 expr2


  eval (EAdd _ expr1 (OPlus _) expr2) = ExprU.evalVIntExpr (+) expr1 expr2

  eval (EAdd _ expr1 (OMinus _) expr2) = ExprU.evalVIntExpr (-) expr1 expr2


  eval (ERel _ expr1 (OLth _) expr2) = ExprU.evalVIntToVBoolExpr (<) expr1 expr2

  eval (ERel _ expr1 (OLE _) expr2) = ExprU.evalVIntToVBoolExpr (<=) expr1 expr2

  eval (ERel _ expr1 (OGth _) expr2) = ExprU.evalVIntToVBoolExpr (>) expr1 expr2

  eval (ERel _ expr1 (OGE _) expr2) = ExprU.evalVIntToVBoolExpr (>=) expr1 expr2

  eval (ERel _ expr1 (OEq _) expr2) = ExprU.evalVIntToVBoolExpr (==) expr1 expr2

  eval (ERel _ expr1 (ONe _) expr2) = ExprU.evalVIntToVBoolExpr (/=) expr1 expr2


  eval (EAnd _ expr1 expr2) = ExprU.evalVBoolExpr (&&) expr1 expr2

  eval (EOr _ expr1 expr2) = ExprU.evalVBoolExpr (||) expr1 expr2


  eval (EVar _ name) = do
    pers <- get
    pure $ getValue name pers

  eval (EApp _ name arguments) = ExprU.evalBuildinAppOrRegular name arguments $ do
    pers <- get

    argVals <- mapM eval arguments
    argLocs <- mapM ExprU.getArgLocation arguments

    let env = getEnv pers
    let function = getValue name pers
    let functionEnv = getFunctionEnv function
    let functionArgs = getFunctionArgs function
    let functionBlock = getFunctionBlock function

    modify $ putEnv functionEnv
    modify $ putValue name function
    modify $ putReturnValue
    mapM_ ExprU.putArgsToPers (zip3 functionArgs argVals argLocs)

    eval functionBlock

    pers <- get
    let returnValue = getReturnValue pers

    modify $ putEnv env
    pure returnValue

  eval (ELambda _ arguments _ block) = do
    pers <- get
    pure $ VFun arguments block (getEnv pers)





