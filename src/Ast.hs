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
  | MCons Access Purity [TypedName] Block
  | MVar Access Var
  deriving(Eq, Show)

data Access
  = Pub
  | Pro
  | Pri
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
  = SAssign Lexpr Expr
  | SVar Var
  | SFunc Func
  | SIf IfChain
  | SExpr Expr
  deriving(Eq, Show)

data Var
  = Var TypedName Expr
  deriving(Eq, Show)

data Func
  = Func Sig Block
  deriving(Eq, Show)

data Sig
  = Sig Name AnonSig
  deriving(Eq, Show)

data Purity
  = Pure          -- A function that operates independently of global state
  | Impure        -- A function that may read global state
  | SideEffecting -- A function that may read or write global state
  deriving(Eq, Show)

-- Expressions that can appear on the left side of an assignment
data Lexpr
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

  | ELitBln Bool
  | ELitChr Char
  | ELitFlt Float
  | ELitInt Int
  | ELitStr String

  deriving(Eq, Show)

data IfChain
  = IfChainIf CondBlock IfChain
  | IfChainElse Block
  | IfChainNone
  deriving(Eq, Show)

data CondBlock
  = CondBlock Expr Block
  deriving(Eq, Show)

data Lambda
  = Lambda AnonSig Block
  deriving(Eq, Show)

-- Anonymous function signature
data AnonSig
  = AnonSig Purity [TypedName] Type
  deriving(Eq, Show)

data TypedName
  = TypedName Type Name
  deriving(Eq, Show)

data Apply
  = Apply Expr [Expr]
  deriving(Eq, Show)

data Cons
  = Cons Typename [Expr]
  deriving(Eq, Show)

data Select
  = Select Expr Name
  deriving(Eq, Show)

type Name = String

