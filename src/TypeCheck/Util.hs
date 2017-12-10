module TypeCheck.Util where

import Ast.A2Constrained

typeOfExpr :: Expr -> Type
typeOfExpr (Expr t _) = t

typeOfFunc :: Func -> Type
typeOfFunc (Func (Sig purity params returnType) _) =
  TFunc purity (map (\(Param _ t _) -> t) params) returnType

typeOfVar :: Var -> Type
typeOfVar (Var _ t _) = t

