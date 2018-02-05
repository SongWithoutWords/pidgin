module TypeCheck.Constraint
  ( module TypeCheck.Constraint
  -- , module Ast.A2Constrained.Type
  ) where

import Ast.A2Constrained -- .Type

type Constraints = [Constraint]

infixr 0 :$=
data Constraint
  = Type :$= Type -- constraint arising from value assignment
  | Type :&= Type -- constraint arising from reference assignment
  deriving(Eq, Ord, Show)

mapConstraint :: (Type -> Type) -> Constraint -> Constraint
mapConstraint f (t1 :$= t2) = (f t1) :$= (f t2)
mapConstraint f (t1 :&= t2) = (f t1) :&= (f t2)

