-- {-# LANGUAGE GADTs #-}

module Syntax where

import Tokens()

-- Finite number of steps friend!

data Module
  = Module [TopLevel]
  deriving(Eq, Show)

data TopLevel
  = Namespace Name [TopLevel]
  | TopLevelClass Class
  | TopLevelFunction Function
  deriving(Eq, Show)

data Class
  = Class Name [Parameter] [Member]
  deriving(Eq, Show)

data Member
  = MemberClass AccessMod Class
  | MemberFunction AccessMod Mutability Function
  | MemberVariable AccessMod Variable
  deriving(Eq, Show)

data AccessMod
  = Pub | Pro | Pri
  deriving(Eq, Show)

-- TODO: add support for templated types and functions :)
data Type
  = TempRef Mutability Type
  | PersRef Mutability Type
  | Option Mutability Type
  | DataType Mutability DataType
  | ZeroOrMore Type -- Cannot be mutable
  | OneOrMore Type  -- Cannot be mutable
  deriving(Eq, Show)

data Mutability
  = Mutable     -- Mutable in present scope
  | Immutable   -- Not mutable in present scope
  -- Constant   -- Not mutable in any scope - planned
  -- CtConstant -- Known at compile time - planned
  deriving(Eq, Show)

data DataType
  = TypeBln
  | TypeChr
  | TypeFlt
  | TypeInt
  | TypeNat
  | TypeStr
  | TypeUser TypeName
  | TypeInferred
  deriving(Eq, Show)

data TypeName
  = TypeName String
  deriving(Eq, Show)

data Block
  = Block [Stmt]
  deriving(Eq, Show)

data Stmt
  = StmtAssign Lexpr Expr
  | StmtVariable Variable
  | StmtFunction Function
  | StmtIf IfStruct
  | StmtApply Apply
  deriving(Eq, Show)

data Variable
  = Variable Type Name Expr
  deriving(Eq, Show)

data Function
  = Function Signature Block
  deriving(Eq, Show)

data Signature
  = Signature Purity Name [Parameter] Type
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
  = ExprIf IfStruct
  | ExprLambda Lambda
  | ExprApply Apply
  | ExprAccess Access
  | ExprName Name
  | ExprLit Lit
  deriving(Eq, Show)

data IfStruct
  = IfStruct CondBlock [CondBlock] (Maybe Block) -- if [else if] else?
  deriving(Eq, Show)

data CondBlock
  = CondBlock Expr Block
  deriving(Eq, Show)

-- Not sure what to do about lambdas and purity. Capture lists?
data Lambda
  = Lambda [Parameter] Block
  deriving(Eq, Show)

data Parameter
  = Parameter Type Name
  deriving(Eq, Show)

data Apply
  = Apply Expr [Expr]
  deriving(Eq, Show)

data Access
  = Access Expr Name
  deriving(Eq, Show)

data Name
  = Name String
  deriving(Eq, Show)

data Lit
  = LitBln Bool
  | LitChr Char
  | LitFlt Float
  | LitInt Int
  | LitStr String
  deriving(Eq, Show)

