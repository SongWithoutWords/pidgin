-- Asts are numbered sequentially, and represent the program at different phases of compilation
-- To understand the changes between different phases of compilation, it's best to diff the Asts
-- Ast2 represents the Ast after it has been constrained for type checking, but before unification
module Ast.A2Constrained
  ( module Ast.A2Constrained
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

data Unit
  = UNamespace (Table Unit)
  | UData Data
  | UFunc Func
  | UVar Var
  deriving(Eq, Show)

type Data = Table Member

data Member
  = MData Access Data
  | MVar Access Type
  deriving(Eq, Ord, Show)

data Func = Func Sig Block
  deriving(Eq, Show)

data Sig = Sig Purity Params Type
  deriving(Eq, Show)

-- In future Params could be alias for [Named MType]
type Params = [Param]

type Param = Named Type

type Block = [Expr]

data Var = Var Type Expr
  deriving(Eq, Show)

data Expr = Expr Type Expr'
  deriving(Eq, Show)

data Expr'
  = ELambda Func

  | EIf Expr Block Block
  | ERet Expr

  | EVar (Named Var)
  | EAssign Expr Expr

  | EApp Expr Purity [Expr]
  | EOver [Expr] -- Expression representing an overloaded name

  | ESelect Expr Name
  | EName Name

  | EIntr Intrinsic
  | EVal Value
  deriving(Eq, Show)

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
  | TOver TVar Types

  | TRef Type
  | TArray Type

  | TUser Typename Data
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

