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

  | TBln Mut
  | TChr Mut
  | TFlt Mut
  | TInt Mut
  | TNat Mut
  | TStr Mut

  | TNone

  deriving(Eq, Show)

data Mut
  = Mutable     -- Mutable in present scope
  | Immutable   -- Not mutable in present scope
  -- Constant   -- Not mutable in any scope - planned
  -- CtConstant -- Known at compile time - planned
  deriving(Eq, Show)

type Block = [Stmt]

data Stmt
  = SAssign LExpr Expr
  | SVar Var
  | SFunc Func
  | SIf IfBranch
  | SExpr Expr
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
  = Var TypedName Expr
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
  = EApply Expr Purity [Expr]
  | ESelect Expr Name
  | EName Name
  deriving(Eq, Show)

type Typename = String

type Name = String

