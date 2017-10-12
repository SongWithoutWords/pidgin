{-# language GADTs #-}

module TypeCheck.Unify
  ( unifyConstraints
  , module TypeCheck.Constraint
  , module TypeCheck.ErrorM
  , module TypeCheck.Substitution
  ) where

import Data.Map as M

import TypeCheck.Constraint
import TypeCheck.ErrorM
import TypeCheck.Substitution

unifyConstraints :: [Constraint] -> ErrorM Substitutions
unifyConstraints [] = pure M.empty
unifyConstraints (c : cs) = do
  s2 <- unifyConstraints cs
  s1 <- unifyConstraint $ subConstraint s2 c
  let s2' = M.map (subType s1) s2
  pure $ M.union s1 s2'

unifyConstraint :: Constraint -> ErrorM Substitutions
unifyConstraint (t1 := t2) = let
  unify' :: Type2 -> Type2 -> ErrorM Substitutions
  unify' a b

    | a == b = pure M.empty

    | TVar a' <- a = pure $ M.singleton a' b
    | TVar b' <- b = pure $ M.singleton b' a

    | TError a' <- a = raise a' >> pure M.empty
    | TError b' <- b = raise b' >> pure M.empty

    -- TODO: Handle purity
    | TFunc _ aParams aRet <- a
    , TFunc _ bParams bRet <- b
      = do
          paramConstraints <- constrainParamTypes aParams bParams
          unifyConstraints $ paramConstraints ++ [aRet := bRet]

    | otherwise = raise (failedToUnify a b) >> pure M.empty

  in unify' t1 t2

constrainParamTypes :: [Type2] -> [Type2] -> ErrorM [Constraint]
constrainParamTypes xs ys = if length xs == length ys
  then pure $ zipWith (\x y -> x := y) xs ys
  else raise (WrongNumArgs (length xs) (length ys)) >> pure []

subConstraint :: Substitutions -> Constraint -> Constraint
subConstraint = mapConstraint . subType

