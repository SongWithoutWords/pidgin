module Ast where

import Tokens()

-- Finite number of steps friend!

type Ast = [Unit]

data Unit
  = UNamespace Name [Unit]
  | UClass Class
  | UFunc Func
  | UVar Var -- May want to disallow global mutable variables
  deriving(Eq, Show)

data Class
  = Class Name [Member]
  deriving(Eq, Show)

data Member
  = MClass Access Class
  | MFunc Access Mut Func
  | MCons Access Lambda
  | MVar Access Var
  deriving(Eq, Show)

data Access
  = Pub
  | Pro
  | Pri
  deriving(Eq, Show)

data Func
  = Func Name Lambda
  deriving(Eq, Show)

data Lambda
  = Lambda Sig Block
  deriving(Eq, Show)

data Sig
  -- Callee cares about left-most mutability of param types but not return
  = Sig Purity [MTypeName] Type
  deriving(Eq, Show)

data Purity
  = Pure
  | PRead
  | PWrite
  deriving(Eq, Show)

type MTypeName = (MType, Name)
type TypedName = (Type, Name)

type MType = (Mut, Type)

data Mut
  = Mut         -- Mutable in present scope
  | Imut        -- Immutable in present scope
  -- Constant   -- Not mutable in any scope - planned
  -- CtConstant -- Known at compile time - planned
  deriving(Eq, Show)

data Type
  = TUser Typename

  -- Neither caller nor callee need care about the left-most mutability of parameter and return types
  | TFunc Purity [Type] Type

  | TTempRef MType
  | TPersRef MType
  | TOption MType
  | TZeroPlus MType
  | TOnePlus MType

  | TBln
  | TChr
  | TFlt
  | TInt
  | TNat
  | TStr

  | TNone
  | TInferred

  deriving(Eq, Show)

type Block = [Stmt]

data Stmt
  = SAssign LExpr Expr
  | SVar Var
  | SFunc Func
  | SIf IfBranch
  | SApply Expr Purity [Expr]
  -- | SExpr Expr
  deriving(Eq, Show)

data IfBranch
  = Iff CondBlock
  | IfElse CondBlock Block
  | IfElif CondBlock IfBranch
  deriving(Eq, Show)

data CondBlock
  = CondBlock Expr Block
  deriving(Eq, Show)

data Var
  = Var MTypeName Expr
  deriving(Eq, Show)

data Expr
  = LExpr LExpr

  | EIf Expr {- if -} Expr {- else -} Expr
  | ELambda Lambda
  | ECons Typename Purity [Expr]

  -- Unary operators
  | ENegate Expr

  -- Binary operators
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EGreater Expr Expr
  | ELesser Expr Expr
  | EGreaterEq Expr Expr
  | ELesserEq Expr Expr

  -- Literals
  | ELitBln Bool
  | ELitChr Char
  | ELitFlt Float
  | ELitInt Int
  | ELitStr String

  deriving(Eq, Show)

-- Expressions that can appear on the left side of an assignment
data LExpr
  = EApply Apply
  -- = EApply Expr Purity [Expr]
  | ESelect Expr Name
  | EName Name
  deriving(Eq, Show)

data Apply
  = Apply Expr Purity [Expr]
  deriving(Eq, Show)

type Typename = String

type Name = String

