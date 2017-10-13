{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}

module Ast
  ( module Ast
  , module Ast.Error
  , module Ast.Name
  , module Ast.Op
  , module Ast.Type
  ) where

import MultiMap

import Ast.Error
import Ast.Name
import Ast.Op
import Ast.Phases
import Ast.Type

type Table a = MultiMap Name a

type Ast0 = [Named Unit0] -- Parse tree
type Ast1 = Table Unit1   -- Ast
type Ast2 = Table Unit2   -- Type-checked Ast

type Unit0 = Unit 'A0 'B0
type Unit1 = Unit 'A1 'B0
type Unit2 = Unit 'A1 'B1
data Unit :: A -> B -> * where

  UNamespace0 :: [Named (Unit 'A0 b)] -> Unit 'A0 b
  UNamespace1 :: Table (Unit 'A1 b) -> Unit 'A1 b

  UClass :: Class a b -> Unit a b
  UFunc :: Func a b -> Unit a b
  UVar :: Var a b -> Unit a b

deriving instance Eq (Unit a b)
deriving instance Show (Unit a b)

-- Finite number of steps friend!
  -- Also, bring back MType!

type Class0 = Class 'A0 'B0
type Class1 = Class 'A1 'B0
type Class2 = Class 'A1 'B1
data Class :: A -> B -> * where
  Class0 :: [Named (Member 'A0 b)] -> Class 'A0 b
  Class1 :: Table (Member 'A1 b) -> Class 'A1 b

deriving instance Eq (Class a b)
deriving instance Show (Class a b)


type Member0 = Member 'A0 'B0
type Member1 = Member 'A1 'B0
type Member2 = Member 'A1 'B1
data Member :: A -> B -> * where
  MClass :: Access -> Class a b -> Member a b

  MFunc :: Access -> Mut -> Func a b -> Member a b

  MCons :: Access -> Func a b -> Member a b
  MVar :: Access -> Var a b -> Member a b

deriving instance Eq (Member a b)
deriving instance Show (Member a b)


data Access
  = Pub
  | Pro
  | Pri
  deriving(Eq, Show)


data RetNotation
  = ImplicitRet
  | ExplicitRet
  deriving(Eq, Show)

type Func0 = Func 'A0 'B0
type Func1 = Func 'A1 'B0
type Func2 = Func 'A1 'B1

data Func :: A -> B -> * where

  -- ImplicitRets are replaced by a return field on block during post-parsing
  Func0 :: Sig b -> RetNotation -> Block 'A0 b -> Func 'A0 b
  Func1 :: Sig b -> Block 'A1 b -> Func 'A1 b

deriving instance Eq (Func a b)
deriving instance Show (Func a b)


type Sig0 = Sig 'B0
type Sig2 = Sig 'B1
data Sig :: B -> * where
  Sig0 :: Purity -> Params0 -> Maybe Type0 -> Sig0
  Sig2 :: Purity -> Params2 -> Type2 -> Sig2

deriving instance Eq (Sig b)
deriving instance Show (Sig b)

-- In future Params could be alias for [Named MType]
type Params0 = Params 'B0
type Params2 = Params 'B1
type Params b = [Param b]

type Param0 = Param 'B0
type Param2 = Param 'B1
data Param :: B -> * where
  Param :: Mut -> Type b -> Name -> Param b
  deriving(Eq, Show)


type Block0 = Block 'A0 'B0
type Block1 = Block 'A1 'B0
type Block2 = Block 'A1 'B1
data Block :: A -> B -> * where
  Block0 :: [Stmt 'A0 'B0] -> Block 'A0 'B0
  Block1 :: [Stmt 'A1 b] -> Maybe (Expr 'A1 b) -> Block 'A1 b

deriving instance Eq (Block a b)
deriving instance Show (Block a b)


type Stmt0 = Stmt 'A0 'B0
type Stmt1 = Stmt 'A1 'B0
type Stmt2 = Stmt 'A1 'B1
data Stmt :: A -> B -> * where

  SAssign :: LExpr a b -> Expr a b -> Stmt a b
  SVar :: Named (Var a b) -> Stmt a b
  SFunc :: Named (Func a b) -> Stmt a b
  SIf :: IfBranch a b -> Stmt a b

  -- SExprs and SRets are replaced by SApps and ret field on block during post-parsing
  SExpr :: Expr 'A0 b -> Stmt 'A0 b
  SRet :: Expr 'A0 b -> Stmt 'A0 b

  SApp :: App 'A1 b -> Stmt 'A1 b

deriving instance Eq (Stmt a b)
deriving instance Show (Stmt a b)


data IfBranch :: A -> B -> *  where
  Iff :: CondBlock a b -> IfBranch a b
  IfElse :: CondBlock a b -> Block a b -> IfBranch a b
  IfElif :: CondBlock a b -> IfBranch a b -> IfBranch a b

deriving instance Eq (IfBranch a b)
deriving instance Show (IfBranch a b)

data CondBlock :: A -> B -> * where
  CondBlock :: Expr a b -> Block a b -> CondBlock a b

deriving instance Eq (CondBlock a b)
deriving instance Show (CondBlock a b)

type Var0 = Var 'A0 'B0
type Var1 = Var 'A1 'B0
type Var2 = Var 'A1 'B1
data Var :: A -> B -> * where

  Var0 :: Mut -> Maybe Type0 -> Expr a 'B0 -> Var a 'B0
  Var2 :: Mut -> Type2 -> Expr a 'B1 -> Var a 'B1

deriving instance Eq (Var f c)
deriving instance Show (Var f c)

type Expr0 = Expr 'A0 'B0
type Expr1 = Expr 'A1 'B0
type Expr2 = Expr 'A1 'B1
data Expr :: A -> B -> * where

  -- Each expression is annotated with a type after type-checking
  Expr0 :: Expr' a 'B0 -> Expr a 'B0
  Expr2 :: Type2 -> Expr2' -> Expr2

deriving instance Eq (Expr a b)
deriving instance Show (Expr a b)

type Expr0' = Expr' 'A0 'B0
type Expr1' = Expr' 'A1 'B0
type Expr2' = Expr' 'A1 'B1
data Expr' :: A -> B -> * where

  EApp :: App a b -> Expr' a b
  ESelect :: Select a b -> Expr' a b
  EName :: Name -> Expr' a b

  -- If-expression of the form "a if b else c"
  EIf :: Expr a b -> Expr a b -> Expr a b -> Expr' a b

  ELambda :: Func a b -> Expr' a b
  ECons :: Typename -> Args a b -> Expr' a b

  -- Should be replaced by EApp?
  EUnOp :: UnOp -> Expr a b -> Expr' a b
  EBinOp :: BinOp -> Expr a b -> Expr a b -> Expr' a b

  EVal :: Value -> Expr' a b


deriving instance Eq (Expr' a b)
deriving instance Show (Expr' a b)

data Value
  = VBln Bool
  | VChr Char
  | VFlt Float
  | VInt Int
  | VStr String
  deriving(Eq, Show)

type LExpr0 = LExpr 'A0 'B0
type LExpr1 = LExpr 'A1 'B0
type LExpr2 = LExpr 'A1 'B1
data LExpr :: A -> B -> * where

  LExpr0 :: LExpr' a 'B0 -> LExpr a 'B0
  LExpr2 :: Type2 -> LExpr2' -> LExpr2

deriving instance Eq (LExpr a b)
deriving instance Show (LExpr a b)

-- LExpr are a subset of Expr that can appear on the left side of an assignment
type LExpr0' = LExpr' 'A0 'B0
type LExpr1' = LExpr' 'A1 'B0
type LExpr2' = LExpr' 'A1 'B1
data LExpr' :: A -> B -> * where
  LApp :: App a b -> LExpr' a b
  LSelect :: Select a b -> LExpr' a b
  LName :: Name -> LExpr' a b

deriving instance Eq (LExpr' a b)
deriving instance Show (LExpr' a b)

type App0 = App 'A0 'B0
type App1 = App 'A1 'B0
type App2 = App 'A1 'B1
data App :: A -> B -> * where
  App :: Expr a b -> Args a b -> App a b

deriving instance Eq (App a b)
deriving instance Show (App a b)


type Args0 = Args 'A0 'B0
type Args1 = Args 'A1 'B0
type Args2 = Args 'A1 'B1
data Args :: A -> B -> * where
  Args :: Purity -> [Expr a b] -> Args a b

deriving instance Eq (Args a b)
deriving instance Show (Args a b)


type Select0 = Select 'A0 'B0
type Select1 = Select 'A1 'B0
type Select2 = Select 'A1 'B1
data Select :: A -> B -> * where
  Select :: Expr a b -> Name -> Select a b

deriving instance Eq (Select a b)
deriving instance Show (Select a b)

