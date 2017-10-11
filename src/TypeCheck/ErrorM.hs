module ErrorM where

import Control.Monad.Writer

import Ast.Type

type ErrorM a = Writer [Error] a

raise :: Error -> ErrorM ()
raise = tell . pure

