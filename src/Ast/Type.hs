{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}

module Ast.Type where

import Ast.Phases


data Kind
  = KNamespace
  | KType
  | KExpr Type2

type Type0 = Type 'B0
type Type2 = Type 'B1
data Type :: B -> * where
  TUser :: Typename -> Type b

  -- Neither caller nor callee care about left-most mutability of param and return types
  TFunc :: Purity -> [Type b] -> Type b -> Type b

  TTempRef :: Mut -> Type b -> Type b
  TPersRef :: Mut -> Type b -> Type b

  TOption :: Mut -> Type b -> Type b
  TZeroPlus :: Mut -> Type b -> Type b
  TOnePlus :: Mut -> Type b -> Type b

  TBln :: Type b
  TChr :: Type b
  TFlt :: Type b
  TInt :: Type b
  TNat :: Type b
  TStr :: Type b

  TNone :: Type b

  -- Type errors can only occur after the ast has been type checked
  TError :: Type2

  -- A type-level meta-variable
  TVar :: Word -> Type2

deriving instance Eq (Type b)
deriving instance Show (Type b)

type Typename = String

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

