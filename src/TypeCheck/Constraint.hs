module TypeCheck.Constraint
  ( module TypeCheck.Constraint
  , module Ast.A2Constrained.Type
  ) where

import Ast.A2Constrained.Type

data Constraint
  = Type :$= Type -- constraint arising from value assignment
  deriving(Eq, Ord, Show)

mapConstraint :: (Type -> Type) -> Constraint -> Constraint
mapConstraint f (t1 :$= t2) = (f t1) :$= (f t2)

