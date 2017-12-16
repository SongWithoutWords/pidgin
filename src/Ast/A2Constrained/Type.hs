module Ast.A2Constrained.Type
  ( module Ast.A2Constrained.Type
  , module Ast.Common.Mutability
  , module Ast.Common.Purity
  ) where

import Ast.Common.Mutability
import Ast.Common.Purity

data Type
  -- = TUser Typename
  = TMut Type

  -- Neither caller nor callee care about left-most mutability of param and return types
  | TFunc Purity [Type] Type

  | TRef Type

  | TUser Typename
  | TBln
  | TChr
  | TFlt
  | TInt
  | TNat
  | TStr

  | TNone

  | TError
  | TVar TVar

  deriving(Eq, Ord, Show)

type TVar = Word

type Typename = String

