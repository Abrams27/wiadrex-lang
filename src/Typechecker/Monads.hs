module Typechecker.Monads where

import Prelude
import Typechecker.Data.Environment
import Typechecker.Data.Exceptions
import Typechecker.Data.Types
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Syntax.AbsWiadrexLang

type TypecheckerM = TypecheckerM' ()
type TypecheckerM' a = StateT Env (Except TypecheckingException) a

type TypegetterM = TypegetterM' RawType
type EmptyTypegetterM = TypegetterM' ()
type TypegetterM' a = ReaderT Env (Except TypecheckingException) a


class Typechecker a where

  checkTypeM :: Maybe RawType -> a -> TypecheckerM


class Typegetter a where

  getTypeM :: a -> TypegetterM
