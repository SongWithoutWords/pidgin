module TypeCheck.Kind where

import Ast.A2Constrained.Type

data Kind
  = KNamespace
  | KType
  | KVar Type

