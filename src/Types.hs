module Types where

data Type
  = TUser Typename

  -- Neither caller nor callee need care about the left-most mutability of parameter and return types
  | TFunc Purity [Type] (Maybe Type)

  | TTempRef Mut Type
  | TPersRef Mut Type

  | TOption Mut Type
  | TZeroPlus Mut Type
  | TOnePlus Mut Type

  | TBln
  | TChr
  | TFlt
  | TInt
  | TNat
  | TStr

  | TNone

  deriving(Eq, Ord, Show)

data Purity
  = Pure
  | PRead
  | PWrite
  deriving(Eq, Ord, Show)

data Mut
  = Mut         -- Mutable in present scope
  | Imut        -- Immutable in present scope
  -- Constant   -- Not mutable in any scope - planned
  -- CtConstant -- Known at compile time - planned
  deriving(Eq, Ord, Show)

type Typename = String
