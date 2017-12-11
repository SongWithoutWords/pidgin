module Ast.Common.Mutability where

data Mut
  = Mut         -- Mutable in present scope
  | Imt        -- Immutable in present scope
  -- Constant   -- Not mutable in any scope - planned
  -- CtConstant -- Known at compile time - planned
  deriving(Eq, Ord, Show)

