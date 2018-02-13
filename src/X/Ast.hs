-- Asts are numbered sequentially, and represent the program at different phases of compilation
-- To understand the changes between different phases of compilation, it's best to diff the Asts
-- Ast2 represents the Ast after it has been constrained for type checking, but before unification
module X.Ast
  ( module X.Ast
  , module Ast.Common.Access
  , module Ast.Common.Intrinsic
  , module Ast.Common.Mutability
  , module Ast.Common.Purity
  , module Ast.Common.Table
  , module Ast.Common.Value
  ) where

-- import Ast.A2Constrained.Type
import Ast.Common.Access
import Ast.Common.Intrinsic
import Ast.Common.Mutability
import Ast.Common.Purity
import Ast.Common.Table
import Ast.Common.Value

-- Finite number of steps friend!

type Ast = Table Unit

type Units = Table Unit

data Unit
  = UNamespace Units
  | UData Members
  | UFunc Func
  | UVar Expr
  deriving(Eq, Ord, Show)

type Members = Table Member

data Member
  = MData Access Members
  | MVar Access Type
  deriving(Eq, Ord, Show)

data Func = Func Sig Block
  deriving(Eq, Ord, Show)

data Sig = Sig Purity Params Type
  deriving(Eq, Ord, Show)

-- In future Params could be alias for [Named MType]
type Params = [Param]

type Param = Named Type

type Block = [Expr]

data Kind
  = KNamespace Units
  | KType Type
  | KExpr Expr
  deriving(Eq, Ord, Show)

-- Would "Term", "Symbol" or "Node" be clearer as a lookup result than kind? Just an idea
-- data Term
--   = TNamespace
--   | TType
--   | TExpr
--   | TFunc

type Kinds = [Kind]

-- Starting to doubt that I need/want types for each expr this at this compliation stage...
data Expr
  = Expr Type Expr'
  | ERef ERef -- A numbered reference to another expr
  deriving(Eq, Ord, Show)

data Expr'
  = EApp Purity Expr [Expr]
  | EAssign Expr Expr
  | EBinding
  | ECons Expr -- Dummy expr used to constrain types
  | EIf Expr Block Block
  | EIntr Intrinsic
  | ELambda Func
  | EName Name Kinds
  | ERet Expr
  | ESelect Expr Name Kinds
  | EVal Value
  | EVar Name Expr
  deriving(Eq, Ord, Show)

type ERef = Int

-- For all intents and purposes, I think I only care either when the type or value is known
-- Would it be possible to get rid of TVar's and use this instead? Possibly not... at all.
data ERefResult -- but maybe expr's better still
  = Dynamic Type
  | Static Value

type Types = [Type]

infixr 0 ~>
(~>) :: [Type] -> Type -> Type
ts ~> t = TFunc Pure ts t

data Type
  -- = TUser Typename
  = TMut Type

  -- Neither caller nor callee care about left-most mutability of param and return types
  | TFunc Purity Types Type

  -- Types associated with an overloaded name
  -- | TOver TVar Types
  | TSuper Types

  | TRef Type
  | TArray Type

  | TData Name Members

  | TUser Name Kinds
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

type TVar = Int

type Typename = String

