module Ast.A2Constrained.Type
  ( module Ast.A2Constrained.Type
  , module Ast.Common.Mutability
  , module Ast.Common.Purity
  ) where

import Ast.Common.Mutability
import Ast.Common.Purity

data Type
  = TUser Typename

  -- Neither caller nor callee care about left-most mutability of param and return types
  | TFunc Purity [Type] Type

  | TRef Mut Type

  | TBln
  | TChr
  | TFlt
  | TInt
  | TNat
  | TStr

  | TNone

  | TError
  | TVar Word

  deriving(Eq, Show)

type Typename = String

