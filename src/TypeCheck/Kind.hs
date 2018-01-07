module TypeCheck.Kind where

import Ast.A2Constrained

data Kind
  = KNamespace
  | KType Type
  | KExpr Expr
  deriving(Show)

