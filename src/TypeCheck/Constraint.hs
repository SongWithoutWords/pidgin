module TypeCheck.Constraint
  ( module TypeCheck.Constraint
  , module Ast.A2Constrained.Type
  ) where

import Ast.A2Constrained.Type

data Constraint
  = Type :< Type
  deriving(Eq, Show)

mapConstraint :: (Type -> Type) -> Constraint -> Constraint
mapConstraint f (t1 :< t2) = (f t1) :< (f t2)

class Constrain a where
  (<:) :: a -> a -> Constraint

instance Constrain Type where
  x <: y = x :< y

