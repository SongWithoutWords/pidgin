{-# language GADTs #-}

module TypeCheck.Unify
  ( unify
  , module TypeCheck.Constraint
  , module TypeCheck.ErrorM
  , module TypeCheck.Substitution
  ) where

import Data.Map as M

import TypeCheck.Constraint
import TypeCheck.ErrorM
import TypeCheck.Substitution

unify :: [Constraint] -> (Substitutions, Errors)
unify = runErrorM . unify'

unify' :: [Constraint] -> ErrorM Substitutions
unify' [] = pure M.empty
unify' (c : cs) = do
  s2 <- unify' cs
  s1 <- unifyOne $ subConstraint s2 c
  let s2' = M.map (subType s1) s2
  pure $ M.union s1 s2'

unifyOne :: Constraint -> ErrorM Substitutions
unifyOne (a := b)

  -- equality
  | a == b = pure M.empty

  -- var := any
  | TVar a' <- a = pure $ M.singleton a' b
  | TVar b' <- b = pure $ M.singleton b' a

  -- error := any
  | TError <- a = pure M.empty
  | TError <- b = pure M.empty

  -- function := function
  | TFunc _ aParams aRet <- a
  , TFunc _ bParams bRet <- b
    = do
        paramConstraints <- constrainParams aParams bParams
        unify' $ paramConstraints ++ [aRet := bRet]

  -- function := non-function
  | TFunc _ _ _ <- a = raise (NonApplicable b) >> pure M.empty
  | TFunc _ _ _ <- b = raise (NonApplicable a) >> pure M.empty

  -- inequality
  | otherwise = raise (failedToUnify a b) >> pure M.empty

constrainParams :: [Type2] -> [Type2] -> ErrorM [Constraint]
constrainParams xs ys = if length xs == length ys
  then pure $ zipWith (\x y -> x := y) xs ys
  else raise (WrongNumArgs (length xs) (length ys)) >> pure []

subConstraint :: Substitutions -> Constraint -> Constraint
subConstraint = mapConstraint . subType

