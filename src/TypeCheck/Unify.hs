module TypeCheck.Unify
  ( unify
  , module Ast.A2Constrained.Error
  , module TypeCheck.Constraint
  , module TypeCheck.Substitution
  ) where

import Control.Monad.RWS
import qualified Data.Map as M
import qualified Data.Set as S

import Ast.A2Constrained.Error
import TypeCheck.ApplySubs
import TypeCheck.Constraint
import TypeCheck.Substitution

import Util.Preface


unify :: Constraints -> (Substitutions, Errors)
unify = execUnifyM . unifyAll


type UnifyM = RWS () Errors Substitutions

execUnifyM :: UnifyM a -> (Substitutions, Errors)
execUnifyM unifyM =
  let (substitutions, errors) = execRWS unifyM () mempty
  in (substitutions, subErrors substitutions errors)

raise :: Error -> UnifyM ()
raise err = tell [err]

sub :: TVar -> Type -> UnifyM ()
sub tvar t = modify (M.insert tvar t)


unorderedEq :: (Eq a, Ord a) => [a] -> [a] -> Bool
unorderedEq xs ys = S.fromList xs == S.fromList ys

unifyAll :: [Constraint] -> UnifyM ()
unifyAll [] = pure ()
unifyAll cs = do
  cs' <- concatMapM (applySubs >=> unifyOne) cs
  modify reduceSubs
  if unorderedEq cs cs'
    then raise $ FailedUnification cs
    else unifyAll cs'

applySubs :: Constraint -> UnifyM Constraint
applySubs c = do
  subs <- get
  pure $ subConstraint subs c


unifyOne :: Constraint -> UnifyM Constraints
unifyOne ((TMut a) :$= (TMut b)) = unifyOne (a :$= b)
unifyOne (a :$= (TMut b)) = unifyOne (a :$= b)
unifyOne c@(TMut _ :$= _) = raise (FailedToUnify c) >> pure mempty

unifyOne c@(a :$= b)

  -- equality
  | a == b = pure []

  -- var := any
  | TVar a' <- a = sub a' b >> pure []

  -- any := tvar
  | TVar _ <- b = pure [c]

  -- error :$= any
  | TError <- a = pure []

  -- any :$= error
  | TError <- b = pure []

  -- ref :$= ref
  | TRef a' <- a
  , TRef b' <- b
    = unifyOne $ a' :$= b'

  -- function :$= function
  | TFunc _ aParams aRet <- a
  , TFunc _ bParams bRet <- b = do
      paramConstraints <- constrainParams aParams bParams
      concatMapM unifyOne $ (aRet :$= bRet) : paramConstraints

  -- function :$= non-function
  | TFunc _ _ _ <- a = raise (NonApplicable b) >> pure []

  -- non-function :$= function
  | TFunc _ _ _ <- b = raise (NonApplicable a) >> pure []


  -- TODO: Should probably replace with determination of whether the types may be unifiable

  -- inequality
  | not (typeIsKnown b) = pure [c]
  | otherwise = raise (FailedToUnify c) >> pure []

constrainParams :: [Type] -> [Type] -> UnifyM [Constraint]
constrainParams xs ys = if length xs == length ys
  then pure $ zipWith (:$=) ys xs
  else raise (WrongNumArgs (length ys) (length xs)) >> pure []

typeIsKnown :: Type -> Bool
typeIsKnown (TMut t) = typeIsKnown t
typeIsKnown (TFunc _ params ret) = all typeIsKnown (ret : params)
typeIsKnown (TRef t) = typeIsKnown t
typeIsKnown (TVar _) = False
typeIsKnown _ = True

