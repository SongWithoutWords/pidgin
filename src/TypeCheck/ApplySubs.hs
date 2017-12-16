module TypeCheck.ApplySubs where

import qualified Data.Map as M

import Ast.A2Constrained.Error
import Ast.A2Constrained.Type
import TypeCheck.Constraint
import TypeCheck.Substitution


reduceSubs :: Substitutions -> Substitutions

-- Doesn't seem to work without tying the knot; I don't understand why
reduceSubs s = let s' = M.map (subType s') s in s'


subConstraint :: Substitutions -> Constraint -> Constraint
subConstraint = mapConstraint . subType


subType :: Substitutions -> Type -> Type
subType s = mapTVar $ subTVar s

replaceTVar :: TVar -> Type -> Type -> Type
replaceTVar tvar t = mapTVar (\tvar' -> if tvar' == tvar then t else TVar tvar')

mapTVar :: (TVar -> Type) -> Type -> Type
mapTVar f typ = let mapTVar' = mapTVar f in case typ of
  TVar t -> f t
  TFunc p param ret -> TFunc p (mapTVar' <$> param) (mapTVar' ret)
  TRef t -> TRef $ mapTVar' t
  t -> t

subTVar :: Substitutions -> TVar -> Type
subTVar s x = case M.lookup x s of
  Nothing -> TVar x
  Just (TVar y) -> TVar $ min x y
  Just t -> t


subErrors :: Substitutions -> Errors -> Errors
subErrors = map . subError

subError :: Substitutions -> Error -> Error
subError s = subError'
  where
    subError' (FailedToUnify c) = FailedToUnify $ subConstraint s c
    subError' (FailedToInferType t) = FailedToInferType $ subType s t
    subError' (NonApplicable t) = NonApplicable $ subType s t
    subError' e = e

