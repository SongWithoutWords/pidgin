{-# language GeneralizedNewtypeDeriving #-}

module TypeCheck.ErrorM
  ( module TypeCheck.ErrorM
  , module Ast.A2Constrained.Error
  ) where

import Control.Monad.Writer

import Ast.A2Constrained.Error

newtype ErrorM a = ErrorM (Writer Errors a)
  deriving(Functor, Applicative, Monad)

runErrorM :: ErrorM a -> (a, Errors)
runErrorM (ErrorM w) = runWriter w

raise :: Error -> ErrorM ()
raise = ErrorM . tell . pure

