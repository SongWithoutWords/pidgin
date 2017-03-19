module Syntax where

import Tokens()

-- Finite number of steps friend!

type Root = [Unit]

data Unit
  = UnitNamespace Name [Unit]
  | UnitClass Class
  | UnitFunction Function
  | UnitVariable Variable -- May want to disallow global mutable variables
  deriving(Eq, Show)

data Class
  = Class Name {-[TypedName]-} [Member]
  deriving(Eq, Show)

data Member
  = MemberClass AccessMod Class
  | MemberFunction AccessMod Mutability Function
  | MemberVariable AccessMod Variable
  deriving(Eq, Show)

data AccessMod
  = Pub | Pro | Pri
  deriving(Eq, Show)

-- What happened to type inferred?
data Type

  = TypeUser Mutability Typename
  | TypeFunction Purity [Type] Type
  | TypeInferred Mutability

  | TypeTempRef Mutability Type
  | TypePersRef Mutability Type
  | TypeOption Mutability Type
  | TypeZeroPlus Type
  | TypeOnePlus Type

  | TypeBln Mutability
  | TypeChr Mutability
  | TypeFlt Mutability
  | TypeInt Mutability
  | TypeNat Mutability
  | TypeStr Mutability
  deriving(Eq, Show)

data Mutability
  = Mutable     -- Mutable in present scope
  | Immutable   -- Not mutable in present scope
  -- Constant   -- Not mutable in any scope - planned
  -- CtConstant -- Known at compile time - planned
  deriving(Eq, Show)

type Typename = String

data Block
  = Block [Stmt]
  deriving(Eq, Show)

data Stmt
  = StmtAssign Lexpr Expr
  | StmtVariable Variable
  | StmtFunction Function
  | StmtIf IfChain
  | StmtApply Apply
  deriving(Eq, Show)

data Variable
  = Variable TypedName Expr
  deriving(Eq, Show)

data Function
  = Function Signature Block
  deriving(Eq, Show)

data Signature
  = Signature Name AnonSig
  deriving(Eq, Show)

data Purity
  = Pure          -- A function that operates independently of global state
  | Impure        -- A function that may read global state
  | SideEffecting -- A function that may read or write global state
  deriving(Eq, Show)

-- Expressions that can appear on the left side of an assignment
data Lexpr
  = LexprApply Apply
  | LexprAccess Access
  | LexprName Name
  deriving(Eq, Show)

data Expr
  = ExprIf IfChain
  | ExprLambda Lambda
  | ExprApply Apply
  | ExprAccess Access
  | ExprName Name
  | ExprLit Lit
  deriving(Eq, Show)

data IfChain
  = IfChainIf CondBlock IfChain
  | IfChainElse Block
  | IfChainNone
  deriving(Eq, Show)

data CondBlock
  = CondBlock Expr Block
  deriving(Eq, Show)

-- Not sure what to do about lambdas and purity. Capture lists?
data Lambda
  = Lambda AnonSig Block
  deriving(Eq, Show)

-- Anonymous function signature
data AnonSig
  = AnonSig Purity [TypedName] Type
  deriving(Eq, Show)

data TypedName
  = TypedName Type Name
  deriving(Eq, Show)

data Apply
  = Apply Expr [Expr]
  deriving(Eq, Show)

data Access
  = Access Expr Name
  deriving(Eq, Show)

type Name = String

data Lit
  = LitBln Bool
  | LitChr Char
  | LitFlt Float
  | LitInt Int
  | LitStr String
  deriving(Eq, Show)
