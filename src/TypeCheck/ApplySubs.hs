module TypeCheck.ApplySubs where

import qualified Data.Set as S
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
subType s typ = let subType' = subType s in case typ of
  x@(TVar tvar) -> case M.lookup tvar s of
    Nothing -> x
    Just y -> y
  TOver tvar ts -> case M.lookup tvar s of
    Nothing -> (TOver tvar $ subType' <$> ts)
    Just y -> y
  TFunc p param ret -> TFunc p (subType' <$> param) (subType' ret)
  TMut t -> TMut $ subType' t
  TRef t -> TRef $ subType' t
  TArray t -> TArray $ subType' t
  t -> t

subErrors :: Substitutions -> Errors -> Errors
subErrors = S.map . subError

subError :: Substitutions -> Error -> Error
subError s = subError'
  where
    subError' (FailedToUnify c) = FailedToUnify $ subConstraint s c
    subError' (FailedToInferType t) = FailedToInferType $ subType s t
    subError' (NonApplicable t) = NonApplicable $ subType s t
    subError' (EquallyViableOverloads t ts) =
      EquallyViableOverloads (subType s t) (S.map (subType s) ts)
    subError' e = e

