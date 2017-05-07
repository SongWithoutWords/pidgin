module Ast
  ( module Ast,
    module Types
  ) where

import Types

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
  = Sig Purity Params (Maybe Type)
  deriving(Eq, Show)

type Params = [Param]

data Param
  = Param Mut Type Name
  deriving(Eq, Show)

type Block = [Stmt]

data Stmt
  = SAssign LExpr Expr
  | SVar Var
  | SFunc Func
  | SIf IfBranch
  | SExpr Expr
  | SRet Expr
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
  = Var Mut (Maybe Type) Name Expr
  deriving(Eq, Show)

data Expr
  = EApp App
  | ESelect Select
  | EName Name

  | EIf Expr {- if -} Expr {- else -} Expr
  | ELambda Lambda
  | ECons Typename Args

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
  = LApp App
  | LSelect Select
  | LName Name
  deriving(Eq, Show)

data App
  = App Expr Args
  deriving(Eq, Show)

data Args
  = Args Purity [Expr]
  deriving(Eq, Show)

data Select
  = Select Expr Name
  deriving(Eq, Show)

type Name = String

