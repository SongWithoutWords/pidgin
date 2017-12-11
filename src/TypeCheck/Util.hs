module TypeCheck.Util where

import Ast.A2Constrained

typeOfExpr :: Expr -> Type
typeOfExpr (Expr t _) = t

typeOfFunc :: Func -> Type
typeOfFunc (Func (Sig purity params returnType) _) =
  TFunc purity (map (\(Named _ (MType _ t)) -> t) params) returnType

typeOfVar :: Var -> MType
typeOfVar (Var mt _) = mt

