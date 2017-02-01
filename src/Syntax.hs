module Syntax where

import Tokens

data ExprOrEol
  = Expr Expr
  | Eol
  deriving(Eq, Show)

data Expr
  = Variable String
  | FuncInvoke String [Expr]
  | MethInvoke Expr String [Expr]
  | Lit Lit
  deriving(Eq, Show)

data Lit
  = LitBln Bool
  | LitChr Char
  | LitFlt Float
  | LitInt Int
  | LitStr String
  deriving(Eq, Show)

