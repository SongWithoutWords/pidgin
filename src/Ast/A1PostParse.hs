-- Asts are numbered sequentially, and represent the program at different phases of compilation
-- To understand the changes between different phases of compilation, it's best to diff the Asts
-- Ast1 represents our parse tree after a post-parse cleanup step
module Ast.A1PostParse
  ( module Ast.A1PostParse
  , module Ast.A0Parse.Sig
  , module Ast.A0Parse.Type
  , module Ast.Common.Access
  , module Ast.Common.Table
  , module Ast.Common.Value
  ) where

import Ast.A0Parse.Sig
import Ast.A0Parse.Type
import Ast.Common.Access
import Ast.Common.Table
import Ast.Common.Value

-- Finite number of steps friend!

type Ast = Table Unit

data Unit
  = UNamespace (Table Unit)
  | UClass Class
  | UFunc Func
  | UVar Var
  deriving(Eq, Show)

data Class = Class (Table Member)
  deriving(Eq, Show)

data Member
  = MClass Access Class
  | MFunc Access Mut Func
  | MCons Access Func
  | MVar Access Var
  deriving(Eq, Show)

data Func = Func Sig Exprs
  deriving(Eq, Show)

type Exprs = [Expr]

data Var = Var Mut (Maybe Type) Expr
  deriving(Eq, Show)

data Expr
  = ELambda Func

  | EIf Expr Exprs Exprs
  | ERet Expr

  | EVar (Named Var)
  | EAssign Expr Expr

  | EApp Expr Purity [Expr]
  | ESelect Expr Name
  | EName Name

  | EVal Value
  deriving(Eq, Show)

