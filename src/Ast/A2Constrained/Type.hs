module Ast.A2Constrained.Type
  ( module Ast.A2Constrained.Type
  , module Ast.Common.Mutability
  , module Ast.Common.Purity
  ) where

import Ast.Common.Mutability
import Ast.Common.Purity

data MType = MType Mut Type
  deriving(Eq, Show)

mut :: Type -> MType
mut = MType Mut

imt :: Type -> MType
imt = MType Imt

data Type
  = TUser Typename

  -- Neither caller nor callee care about left-most mutability of param and return types
  | TFunc Purity [Type] Type

  | TRef MType

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

