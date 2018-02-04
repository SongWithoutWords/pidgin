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
  | UData Data
  | UFunc Func
  | UVar Var
  deriving(Eq, Show)

data Data = Data [Named Member]
  deriving(Eq, Show)

data Member
  = MVar Access Type
  | MData Access Data
  deriving(Eq, Show)

data Func = Func Sig Block
  deriving(Eq, Show)

type Block = [Expr]

data Var = Var Mut (Maybe Type) Expr
  deriving(Eq, Show)

data Expr
  = ELambda Func

  | EIf Expr Block Block
  | ERet Expr

  -- Should be ELet? Could I start allowing any expr to be type annotated?
  | EVar (Named Var)
  | EAssign Expr Expr

  | EApp Expr Purity [Expr]
  | ESelect Expr Name
  | EName Name

  | EVal Value
  deriving(Eq, Show)

