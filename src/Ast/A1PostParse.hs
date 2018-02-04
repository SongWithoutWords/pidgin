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
  | UData Data
  | UFunc Func
  | UVar Var
  deriving(Eq, Show)

type Data = Table Member

data Member
  = MData Access Data
  | MVar Access Type
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

  | EVar (Named Var)
  | EAssign Expr Expr

  | EApp Expr Purity [Expr]
  | ESelect Expr Name
  | EName Name

  | EVal Value
  deriving(Eq, Show)

