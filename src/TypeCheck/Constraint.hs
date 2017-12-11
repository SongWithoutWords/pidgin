module TypeCheck.Constraint
  ( module TypeCheck.Constraint
  , module Ast.A2Constrained.Type
  ) where

import Ast.A2Constrained.Type

data Constraint
  = Type :< Type
  | MType ::< MType
  deriving(Eq, Show)

mapConstraint :: (Type -> Type) -> Constraint -> Constraint
mapConstraint f (t1 :< t2) = (f t1) :< (f t2)
mapConstraint f ((MType m1 t1) ::< (MType m2 t2))
  = (MType m1 $ f t1) ::< (MType m2 $ f t2)

class Constrain a where
  (<:) :: a -> a -> Constraint

instance Constrain Type where
  x <: y = x :< y

instance Constrain MType where
  x <: y = x ::< y

