-- Asts are numbered sequentially, and represent the program at different phases of compilation
-- To understand the changes between different phases of compilation, it's best to diff the Asts
-- Ast2 represents the Ast after it has been constrained for type checking, but before unification
module X.Ast
  ( module X.Ast
  , module Ast.Common.Access
  , module Ast.Common.Intrinsic
  , module Ast.Common.Mutability
  , module Ast.Common.Purity
  , module Ast.Common.Table
  , module Ast.Common.Value
  , module Util.StrongIndexVector
  ) where

-- import Data.Vector

-- import Ast.A2Constrained.Type
import Ast.Common.Access
import Ast.Common.Intrinsic
import Ast.Common.Mutability
import Ast.Common.Purity
import Ast.Common.Table
import Ast.Common.Value

-- Finite number of steps friend!

import Util.StrongIndexVector

-- type Ast = Table Unit

-- Should call it scope? Is shorter
type Namespace = Table Unit

data Unit
  = UNamespace Namespace
  | UType TypeId
  | UFunc FuncId
  | UVar VarId
  | UIntr Intrinsic
  deriving(Eq, Ord, Show)

type Units = [Unit]

data Ast = Ast
  { namespace :: Namespace
  , functions :: Functions
  , types :: Types -- Includes local type variables
  , vars :: Vars -- Includes local variables
  }
  deriving(Eq, Ord, Show)

data Data = Data Name Members
  deriving(Eq, Ord, Show)

newtype TypeId = TypeId Int
  deriving(Eq, Ord, Show)

instance Index TypeId where
  toInt (TypeId i) = i

type Types = StrongIndexVector TypeId Type

-- type Members = Table Member


data Member = Member Name Type
  deriving(Eq, Ord, Show)

newtype MId = MId Int
  deriving(Eq, Ord, Show)


type Members = StrongIndexVector MId Member

-- data Member
--   = MData Access Members
--   | MVar Access Type
--   deriving(Eq, Ord, Show)

newtype VarId = VarId Int
  deriving(Eq, Ord, Show)

instance Index VarId where
  toInt (VarId i) = i

type Vars = StrongIndexVector VarId Var

data Var = Var Name Expr
  deriving(Eq, Ord, Show)

newtype FuncId = FuncId Int
  deriving(Eq, Ord, Show)

instance Index FuncId where
  toInt (FuncId i) = i

type Functions = StrongIndexVector FuncId Func

data Func = Func Name Sig Block
  deriving(Eq, Ord, Show)

data Sig = Sig Purity Params Type
  deriving(Eq, Ord, Show)

type Params = [Param]

type Param = Named Type

type Block = [Expr]

-- data Kind
  -- = KNamespace Namespace
  -- | KType TypeId
  -- | KExpr Type
  -- | KVal Value
  -- deriving(Eq, Ord, Show)

-- type Kinds = [Kind]

-- Would "Term", "Symbol" or "Node" be clearer as a lookup result than kind? Just an idea
-- data Id
--   = INamespace Units
--   | IType TypeId
--   | IVar VarId
--   | IFunc FuncId
--   deriving(Eq, Ord, Show)

-- type Ids = [Id]

-- Starting to doubt that I need/want types for each expr this at this compliation stage...
data Expr
  = Expr Type Expr'
  | EType Type
  | ENamespace Namespace
  | EOver [Expr]
  -- | ERef ERef -- A numbered reference to another expr
  deriving(Eq, Ord, Show)

data Expr'
  = EApp Purity Expr [Expr]
  | EAssign Expr Expr
  | EBinding
  | ECons Expr -- Dummy expr used to constrain types
  | EIf Expr Block Block
  | EIntr Intrinsic
  | ELambda Func
  | EName Unit
  -- | EOver [Expr]
  -- | EName Name Units
  | ERet Expr
  | ESelect Expr Name Units
  | EVal Value
  | EVar VarId
  deriving(Eq, Ord, Show)

-- type ERef = Int

-- For all intents and purposes, I think I only care either when the type or value is known
-- Would it be possible to get rid of TVar's and use this instead? Possibly not... at all.
-- data ERefResult -- but maybe expr's better still
--   = Dynamic Type
--   | Static Value

-- type Types = [Type]

infixr 0 ~>
(~>) :: [Type] -> Type -> Type
ts ~> t = TFunc Pure ts t

data Type
  -- = TUser Typename
  = TMut Type

  -- Neither caller nor callee care about left-most mutability of param and return types
  | TFunc Purity [Type] Type

  -- Types associated with an overloaded name
  -- | TOver TVar Types
  | TSuper [Type]

  | TRef Type
  | TArray Type

  | TData Name Members

  | TName Name Units
  | TBln
  | TChr
  | TFlt
  | TInt
  | TNat
  | TStr

  | TNone
  | TUnknown

  | TError
  | TVar TVar

  deriving(Eq, Ord, Show)

type TVar = Int

type Typename = String

