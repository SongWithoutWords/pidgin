module Syntax where

import Tokens

data ExprOrEol = Expr Expr
              | Eol
              deriving(eq, show)

data Expr = Variable String
          | FuncInvoke String [Expr]
          | MethInvoke Expr String [Expr]
          | Lit Lit
          deriving(eq, show)

data Lit = LitBln Bool
         | LitChr Char
         | LitFlt Float
         | LitInt Int
         | LitStr String
         deriving(eq, show)

