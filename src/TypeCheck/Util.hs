{-# language GADTs #-}
module TypeCheck.Util where

import Ast

typeOfExpr :: Expr2 -> Type2
typeOfExpr (Expr2 t _) = t

typeOfFunc :: Func2 -> Type2
typeOfFunc (Func1 (Sig2 purity params returnType) _) =
  TFunc purity (map (\(Param _ t _) -> t) params) returnType

typeOfVar :: Var2 -> Type2
typeOfVar (Var2 _ t _) = t

