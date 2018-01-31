-- Asts are numbered sequentially, and represent the program at different phases of compilation
-- To understand the changes between different phases of compilation, it's best to diff the Asts
-- Ast2 represents the Ast after it has been constrained for type checking, but before unification
module Ast.A2Constrained
  ( module Ast.A2Constrained
  , module Ast.A2Constrained.Type
  , module Ast.Common.Access
  , module Ast.Common.Intrinsic
  , module Ast.Common.Table
  , module Ast.Common.Value
  ) where

import Ast.A2Constrained.Type
import Ast.Common.Access
import Ast.Common.Intrinsic
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

data Sig = Sig Purity Params Type
  deriving(Eq, Show)

-- In future Params could be alias for [Named MType]
type Params = [Param]

type Param = Named Type

type Exprs = [Expr]

data Var = Var Type Expr
  deriving(Eq, Show)

data Expr = Expr Type Expr'
  deriving(Eq, Show)

data Expr'
  = ELambda Func

  | EIf Expr Exprs Exprs
  | ERet Expr

  | EVar (Named Var)
  | EAssign Expr Expr

  | EApp Expr Purity [Expr]
  | EOver [Expr] -- Expression representing an overloaded name

  | ESelect Expr Name
  | EName Name

  | EIntr Intrinsic
  | EVal Value
  deriving(Eq, Show)

