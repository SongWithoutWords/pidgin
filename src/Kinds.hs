module Kinds
  ( module Kinds
  , module Types
  , module TypeErrors
  ) where

import Types
import TypeErrors

data Kind
  = KNamespace
  | KType
  | KExpr Type -- Should be (Either Type [TypeError])? Should have own module?
  | KError Errors

