module Ast.A0Parse.Type
  ( module Ast.A0Parse.Type
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
  | TArray Type

  -- Parameterized type
  | TParam [Typename] Type

  | TBln
  | TChr
  | TFlt
  | TInt
  | TNat
  | TStr

  | TNone

  deriving(Eq, Show)

type Typename = String

