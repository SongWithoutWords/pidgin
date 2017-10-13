{-# language GeneralizedNewtypeDeriving #-}

module TypeCheck.ErrorM
  ( module TypeCheck.ErrorM
  , module Ast.Error
  ) where

import Control.Monad.Writer

import Ast.Error

newtype ErrorM a = ErrorM (Writer Errors a)
  deriving(Functor, Applicative, Monad)

runErrorM :: ErrorM a -> (a, Errors)
runErrorM (ErrorM w) = runWriter w

raise :: Error -> ErrorM ()
raise = ErrorM . tell . pure

