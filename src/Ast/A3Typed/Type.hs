module Ast.A3Typed.Type
  ( module Ast.A3Typed.Type
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

  deriving(Eq, Show)

type Typename = String

