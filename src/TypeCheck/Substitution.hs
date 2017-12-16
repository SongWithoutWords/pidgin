module TypeCheck.Substitution where

import qualified Data.Map as M

import Ast.A2Constrained.Type

type Substitutions = M.Map TVar Type

subType :: Substitutions -> Type -> Type
subType s typ = let subType' = subType s in case typ of

  TFunc p paramTypes retType ->
    TFunc p (subType' <$> paramTypes) (subType' retType)

  TRef t -> TRef $ subType' t

  TVar tvar -> case M.lookup tvar s of
    Nothing -> TVar tvar
    Just t -> t

  t -> t

