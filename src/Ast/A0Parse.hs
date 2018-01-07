-- Asts are numbered sequentially, and represent the program at different phases of compilation
-- To understand the changes between different phases of compilation, it's best to diff the Asts
-- Ast0 represents our initial parse tree
module Ast.A0Parse
  ( module Ast.A0Parse
  , module Ast.A0Parse.Sig
  , module Ast.A0Parse.Type
  , module Ast.Common.Access
  , module Ast.Common.Name
  , module Ast.Common.Value
  ) where

import Ast.A0Parse.Sig
import Ast.A0Parse.Type
import Ast.Common.Access
import Ast.Common.Name
import Ast.Common.Value

-- Finite number of steps friend!

type Ast = [Named Unit]

data Unit
  = UNamespace [Named Unit]
  | UClass Class
  | UFunc Func
  | UVar Var
  deriving(Eq, Show)

data Class = Class [Named Member]
  deriving(Eq, Show)

data Member
  = MClass Access Class
  | MFunc Access Mut Func
  | MCons Access Func
  | MVar Access Var
  deriving(Eq, Show)

data RetNotation
  = ImplicitRet
  | ExplicitRet
  deriving(Eq, Show)

data Func = Func Sig RetNotation Block
  deriving(Eq, Show)

type Block = [Stmt]

data Stmt
  = SAssign Expr Expr
  | SVar (Named Var)
  | SIf IfBranch

  -- SExprs and SRets are replaced by SApps and ret field on block during post-parse
  | SExpr Expr
  | SRet Expr
  deriving(Eq, Show)

data IfBranch
  = If CondBlock
  | IfElse CondBlock Block
  | IfElseIf CondBlock IfBranch
  deriving(Eq, Show)

data CondBlock = CondBlock Expr Block
  deriving(Eq, Show)

data Var = Var Mut (Maybe Type) Expr
  deriving(Eq, Show)

data Expr

  = EApp Expr Purity [Expr]
  | ESelect Expr Name
  | EName Name

  | EIf Cond Expr Expr

  | ELambda Func

  | EVal Value
  deriving(Eq, Show)

newtype Cond = Cond Expr
  deriving(Eq, Show)

