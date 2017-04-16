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
  = Sig Purity [TypedName] Type
  deriving(Eq, Show)

data Purity
  = Pure
  | ReadWorld
  | WriteWorld
  deriving(Eq, Show)

data TypedName
  = TypedName Type Name
  deriving(Eq, Show)

data Type
  = TUser Mut Typename
  | TFunc Purity [Type] Type
  | TInferred Mut
  | TTempRef Mut Type
  | TPersRef Mut Type
  | TOption Mut Type
  | TZeroPlus Type
  | TOnePlus Type

  -- | TWorld Mut

  | TBln Mut
  | TChr Mut
  | TFlt Mut
  | TInt Mut
  | TNat Mut
  | TNone
  | TStr Mut

  deriving(Eq, Show)

data Mut
  = Mutable     -- Mutable in present scope
  | Immutable   -- Not mutable in present scope
  -- Constant   -- Not mutable in any scope - planned
  -- CtConstant -- Known at compile time - planned
  deriving(Eq, Show)

type Typename = String

type Block = [Stmt]

data Stmt
  = SAssign LExpr Expr
  | SVar Var
  | SFunc Func
  | SIf IfChain
  | SExpr Expr
  deriving(Eq, Show)

data Var
  = Var TypedName Expr
  deriving(Eq, Show)

-- Expressions that can appear on the left side of an assignment
data LExpr
  = LApply Apply
  | LSelect Select
  | LName Name
  deriving(Eq, Show)

data Expr
  = EIf Expr {- if -} Expr {- else -} Expr
  | ELambda Lambda
  | EApply Apply
  | ECons Cons
  | ESelect Select
  | EName Name

  -- | EWorld

  -- | LExpr LExpr -- Consider folding LExprs into expressions, and removing apply, cons, select etc.

  -- Consider adding EOp, to avoid some of the ast horror in TestCases.hs

  | ELitBln Bool
  | ELitChr Char
  | ELitFlt Float
  | ELitInt Int
  | ELitStr String

  deriving(Eq, Show)

data IfChain
  = Iff CondBlock
  | IfElse CondBlock Block
  | IfElif CondBlock IfChain
  deriving(Eq, Show)

-- type CondBlock = (Expr, Block)
data CondBlock
  = CondBlock Expr Block
  deriving(Eq, Show)

data Apply
  = Apply Expr Purity [Expr]
  deriving(Eq, Show)

data Cons
  = Cons Typename Purity [Expr]
  deriving(Eq, Show)

data Select
  = Select Expr Name
  deriving(Eq, Show)

type Name = String

