{-# language GADTs #-}

module TypeCheck.Unify
  ( unifyConstraints
  , module TypeCheck.Constraint
  , module TypeCheck.Substitution
  ) where

import Data.Map as M

import TypeCheck.Constraint
import TypeCheck.Substitution

-- TODO: put this in a Writer Error monad, and swap out raise for tell
raise :: Error -> a
raise e = error $ "Unification error " ++ show e

unifyConstraints :: [Constraint] -> Substitutions
unifyConstraints [] = M.empty
unifyConstraints (c : cs) =

  -- TODO: attempt nicer re-write when test-coverage is in place
  let s2 = unifyConstraints cs in
  let s1 = unifyConstraint $ subConstraint s2 c in
  let s2' = M.map (subType s1) s2 in
    M.union s1 s2'

unifyConstraint :: Constraint -> Substitutions
unifyConstraint (t1 := t2) = let
  unify' :: Type2 -> Type2 -> Substitutions
  unify' a b

    | a == b = M.empty

    | TVar aVar <- a = M.singleton aVar b

    | TVar bVar <- b = M.singleton bVar a

    -- TODO: Handle purity
    | TFunc _ aParams aRet <- a
    , TFunc _ bParams bRet <- b
    = unifyConstraints $ constrainParamTypes aParams bParams ++ [aRet := bRet]

    | otherwise = error $ "cannot unify " ++ show a ++ " " ++ show b

  in unify' t1 t2

constrainParamTypes :: [Type2] -> [Type2] -> [Constraint]
constrainParamTypes xs ys = if length xs == length ys
  then zipWith (\x y -> x := y) xs ys
  else raise $ WrongNumArgs (length xs) (length ys)

subConstraint :: Substitutions -> Constraint -> Constraint
subConstraint = mapConstraint . subType

