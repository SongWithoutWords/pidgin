module TypeCheck.Util where

import Ast.A2Constrained

typeOfExpr :: Expr -> Type
typeOfExpr (Expr (MType _ t) _) = t

mTypeOfExpr :: Expr -> MType
mTypeOfExpr (Expr mt _) = mt

typeOfFunc :: Func -> Type
typeOfFunc (Func (Sig purity params returnType) _) =
  TFunc purity (map (\(Named _ (MType _ t)) -> t) params) returnType

typeOfVar :: Var -> Type
typeOfVar (Var (MType _ t) _) = t

mTypeOfVar :: Var -> MType
mTypeOfVar (Var mt _) = mt

