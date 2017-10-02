
module TypeCheck.Unify
  ( unifyConstraints
  , module Constraint
  , module Substitution
  ) where

import Data.Map as M

import TypeCheck.Constraint
import TypeCheck.Substitution

unifyConstraints :: [Constraint] -> Substitutions
unifyConstraints [] = M.empty
unifyConstraints (c : cs) =
  let s2 = unifyConstraints cs in
  let s1 = unifyConstraint $ subConstraint s2 c in
  let s2' = M.map (subType s1) s2 in
    M.union s1 s2'

unifyConstraint :: Constraint -> Substitutions
unifyConstraint (t1 := t2) = let
  unify' :: Type -> Type -> Substitutions
  unify' a b

    | a == b = M.empty

    | TVar x <- a = M.singleton x b

    | TVar y <- b = M.singleton y a

    | TLam x1 x2 <- a
    , TLam y1 y2 <- b
    = unifyConstraints [x1 := y1, x2 := y2]

    | otherwise = error $ "cannot unify " ++ show a ++ " " ++ show b

  in unify' t1 t2

subConstraint :: Substitutions -> Constraint -> Constraint
subConstraint = mapConstraint . subType

