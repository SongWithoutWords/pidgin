module TypeCheck.ErrorM
  ( module TypeCheck.ErrorM
  , module Ast.Error
  , runWriter
  ) where

import Control.Monad.Writer

import Ast.Error

type ErrorM a = Writer [Error] a

raise :: Error -> ErrorM ()
raise = tell . pure

