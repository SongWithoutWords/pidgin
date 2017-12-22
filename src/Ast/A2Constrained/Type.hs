module Ast.A2Constrained.Type
  ( module Ast.A2Constrained.Type
  , module Ast.Common.Mutability
  , module Ast.Common.Purity
  ) where

import Ast.Common.Mutability
import Ast.Common.Purity

type Types = [Type]

data Type
  -- = TUser Typename
  = TMut Type

  -- Neither caller nor callee care about left-most mutability of param and return types
  | TFunc Purity Types Type

  -- Types associated with an overloaded name
  | TOver TVar Types

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

