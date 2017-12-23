module TypeCheck.Kind where

import Ast.A2Constrained

data Kind
  = KNamespace
  | KType
  | KExpr Expr

