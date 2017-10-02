module Constraint
  ( module Constraint
  , module Ast.Type
  ) where

import Ast.Type

data Constraint
  = Type2 := Type2
  deriving(Eq, Show)

mapConstraint :: (Type2 -> Type2) -> Constraint -> Constraint
mapConstraint f (t1 := t2) = (f t1) := (f t2)

