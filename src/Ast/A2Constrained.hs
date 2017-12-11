-- Asts are numbered sequentially, and represent the program at different phases of compilation
-- To understand the changes between different phases of compilation, it's best to diff the Asts
-- Ast2 represents the Ast after it has been constrained for type checking, but before unification
module Ast.A2Constrained
  ( module Ast.A2Constrained
  , module Ast.A2Constrained.Type
  , module Ast.Common.Access
  , module Ast.Common.Op
  , module Ast.Common.Table
  , module Ast.Common.Value
  ) where

import Ast.A2Constrained.Type
import Ast.Common.Access
import Ast.Common.Op
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

data Func = Func Sig Block
  deriving(Eq, Show)

data Sig = Sig Purity Params Type
  deriving(Eq, Show)

-- In future Params could be alias for [Named MType]
type Params = [Param]

type Param = Named MType
--Param MType Name
  -- deriving(Eq, Show)

data Block = Block [Stmt] (Maybe Expr)
  deriving(Eq, Show)

data Stmt
  = SAssign LExpr Expr
  | SVar (Named Var)
  | SFunc (Named Func)
  | SIf IfBranch
  | SApp App
  deriving(Eq, Show)

data IfBranch
  = If CondBlock
  | IfElse CondBlock Block
  | IfElseIf CondBlock IfBranch
  deriving(Eq, Show)

data CondBlock = CondBlock Expr Block
  deriving(Eq, Show)

data Var = Var MType Expr
  deriving(Eq, Show)

data Expr = Expr Type Expr'
  deriving(Eq, Show)

data Expr'
  = EApp App
  | ESelect Select
  | EName Name

  | EIf Cond Expr Expr

  | ELambda Func
  | ECons Typename Args

  -- Should be replaced by EApp?
  | EUnOp UnOp Expr
  | EBinOp BinOp Expr Expr

  | EVal Value
  deriving(Eq, Show)

newtype Cond = Cond Expr
  deriving(Eq, Show)

-- LExpr are a subset of Expr that can appear on the left side of an assignment
data LExpr = LExpr Type LExpr'
  deriving(Eq, Show)

data LExpr'
  = LApp App
  | LSelect Select
  | LName Name
  deriving(Eq, Show)

data App = App Expr Args
  deriving(Eq, Show)

data Args = Args Purity [Expr]
  deriving(Eq, Show)

data Select = Select Expr Name
  deriving(Eq, Show)

