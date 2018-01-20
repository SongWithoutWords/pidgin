module Ast.A0Parse.Type
  ( module Ast.A0Parse.Type
  , module Ast.Common.Mutability
  , module Ast.Common.Purity
  ) where

import Ast.Common.Mutability
import Ast.Common.Purity

infixr 0 ~>
(~>) :: [Type] -> Type -> Type
ts ~> t = TFunc Pure ts t

data Type
  = TUser Typename

  -- Neither caller nor callee care about left-most mutability of param and return types
  | TFunc Purity [Type] Type

  | TRef Mut Type
  | TArray Type

  -- A parameterized type
  | TParams [Typename] Type
  -- A singular type param, could something like this be useful?
  -- | TParam Typename
  -- Arguments to a parameterized type
  | TArgs [Type] Type

  | TBln
  | TChr
  | TFlt
  | TInt
  | TNat
  | TStr

  | TNone

  deriving(Eq, Show)

type Typename = String

