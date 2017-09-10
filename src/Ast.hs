{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}

module Ast
  ( module Ast
  ) where

import qualified Data.Set as Set

import MultiMap

data Storage
  = SList
  | SMap

data TypePhase
  = Typed
  | UnTyped

-- Finite number of steps friend!

type Table a = MultiMap Name a

type AstLu = [UnitLu]
type AstMu = Table UnitMu
type AstMc = Table UnitMc


type UnitLu = Unit 'SList 'UnTyped
type UnitMu = Unit 'SMap 'UnTyped
type UnitMc = Unit 'SMap 'Typed
data Unit :: Storage -> TypePhase -> * where

  UNamespaceL :: Name -> [Unit 'SList ts] -> Unit 'SList ts
  UNamespaceM :: Table (Unit 'SMap ts) -> Unit 'SMap ts

  UClass :: Class col ts -> Unit col ts

  UFuncL :: Func ts -> Unit 'SList ts
  UFuncM :: Lambda ts -> Unit 'SMap ts

  UVar :: Var col ts -> Unit col ts

deriving instance Eq (Unit col tc)
deriving instance Show (Unit col tc)


type ClassLu = Class 'SList 'UnTyped
type ClassMu = Class 'SMap 'UnTyped
type ClassMc = Class 'SMap 'Typed
data Class :: Storage -> TypePhase -> * where
  ClassL :: Name -> [Member 'SList tc] -> Class 'SList tc
  ClassM :: Table (Member 'SMap tc) -> Class 'SMap tc

deriving instance Eq (Class col tc)
deriving instance Show (Class col tc)


type MemberLu = Member 'SList 'UnTyped
type MemberMu = Member 'SMap 'UnTyped
type MemberMc = Member 'SMap 'Typed
data Member :: Storage -> TypePhase -> * where
  MClass :: Access -> Class col tc -> Member col tc
  MFuncL :: Access -> Mut -> Func tc -> Member 'SList tc
  MFuncM :: Access -> Mut -> Lambda tc -> Member 'SMap tc
  MCons :: Access -> Lambda tc -> Member col tc
  MVar :: Access -> Var col tc -> Member col tc

deriving instance Eq (Member col tc)
deriving instance Show (Member col tc)


data Access
  = Pub
  | Pro
  | Pri
  deriving(Eq, Show)

data Func :: TypePhase -> * where
  Func :: Name -> Lambda tc -> Func tc

deriving instance Eq (Func tc)
deriving instance Show (Func tc)

data RetNotation
  = ImplicitRet
  | ExplicitRet
  deriving(Eq, Show)

type LambdaU = Lambda 'UnTyped
type LambdaC = Lambda 'Typed
data Lambda :: TypePhase -> * where
  Lambda :: Sig tc -> RetNotation -> Block tc -> Lambda tc

deriving instance Eq (Lambda tc)
deriving instance Show (Lambda tc)


type SigU = Sig 'UnTyped
type SigC = Sig 'Typed
data Sig :: TypePhase -> * where
  SigU :: Purity -> ParamsU -> Maybe TypeU -> Sig 'UnTyped
  SigC :: Purity -> ParamsT -> TypeT -> Sig 'Typed

deriving instance Eq (Sig tc)
deriving instance Show (Sig tc)


type ParamsU = Params 'UnTyped
type ParamsT = Params 'Typed
type Params tp = [Param tp]

type ParamU = Param 'UnTyped
type ParamT = Param 'Typed
data Param :: TypePhase -> * where
  Param :: Mut -> Type tp -> Name -> Param tp
  deriving(Eq, Show)

-- data Param
--   = Param Mut Type Name
--   deriving(Eq, Show)

type BlockU = Block 'UnTyped
type BlockC = Block 'Typed
type Block tc = [Stmt tc]

-- TODO: some changes to consider
  -- Either limit Stmts to apps and lone exprs only at the end of blocks,
  -- or replace SExpr with SRet or SApp at some stage before code generation

  -- Also consider bringing back Type and MType

type StmtU = Stmt 'UnTyped
type StmtC = Stmt 'Typed
data Stmt :: TypePhase -> * where
  SAssign :: LExpr tc -> Expr tc -> Stmt tc
  SVar :: Var 'SList tc -> Stmt tc
  SFunc :: Func tc -> Stmt tc
  SIf :: IfBranch tc -> Stmt tc
  SExpr :: Expr tc -> Stmt tc
  SRet :: Expr tc -> Stmt tc

deriving instance Eq (Stmt tc)
deriving instance Show (Stmt tc)


data IfBranch :: TypePhase -> *  where
  Iff :: CondBlock tc -> IfBranch tc
  IfElse :: CondBlock tc -> Block tc -> IfBranch tc
  IfElif :: CondBlock tc -> IfBranch tc -> IfBranch tc

deriving instance Eq (IfBranch tc)
deriving instance Show (IfBranch tc)

data CondBlock :: TypePhase -> * where
  CondBlock :: Expr tc -> Block tc -> CondBlock tc

deriving instance Eq (CondBlock tc)
deriving instance Show (CondBlock tc)


type VarLu = Var 'SList 'UnTyped
type VarMu = Var 'SMap 'UnTyped
type VarLc = Var 'SList 'Typed
type VarMc = Var 'SMap 'Typed
data Var :: Storage -> TypePhase -> * where
  VarLu :: Mut -> Maybe TypeU -> Name -> ExprU -> VarLu
  VarMu :: Mut -> Maybe TypeU -> ExprU -> VarMu
  VarLc :: Mut -> TypeT -> Name -> ExprT -> VarLc
  VarMc :: Mut -> TypeT -> ExprT -> VarMc

deriving instance Eq (Var f c)
deriving instance Show (Var f c)

type ExprU = Expr 'UnTyped
type ExprT = Expr 'Typed
data Expr :: TypePhase -> * where
  ExprU :: ExprU' -> ExprU
  ExprT :: TypeT -> ExprT' -> ExprT

deriving instance Eq (Expr tc)
deriving instance Show (Expr tc)

type ExprU' = Expr' 'UnTyped
type ExprT' = Expr' 'Typed
data Expr' :: TypePhase -> * where
  EApp :: App tc -> Expr' tc
  ESelect :: Select tc -> Expr' tc
  EName :: Name -> Expr' tc

  EIf :: Expr tc -> {- if -} Expr tc -> {- else -} Expr tc -> Expr' tc
  ELambda :: Lambda tc -> Expr' tc
  ECons :: Typename -> Args tc -> Expr' tc

  EUnOp :: UnOp -> Expr tc -> Expr' tc
  EBinOp :: BinOp -> Expr tc -> Expr tc -> Expr' tc

  -- Literals
  EValBln :: Bool -> Expr' tc
  EValChr :: Char -> Expr' tc
  EValFlt :: Float -> Expr' tc
  EValInt :: Int -> Expr' tc
  EValStr :: String -> Expr' tc

deriving instance Eq (Expr' tc)
deriving instance Show (Expr' tc)

data UnOp
  = Not
  | Neg
  deriving(Eq, Show)

data BinOp
  = And
  | Or
  | Add
  | Sub
  | Mul
  | Div
  | Greater
  | Lesser
  | GreaterEq
  | LesserEq
  | Equal
  | OpUser String
  deriving(Eq, Ord, Show)


type LExprU = LExpr 'UnTyped
type LExprT = LExpr 'Typed
data LExpr :: TypePhase -> * where
  LExprU :: LExprU' -> LExprU
  LExprT :: TypeT -> LExprT' -> LExprT

deriving instance Eq (LExpr tc)
deriving instance Show (LExpr tc)

-- LExpr are a subset of Expr that can appear on the left side of an assignment
type LExprU' = LExpr' 'UnTyped
type LExprT' = LExpr' 'Typed
data LExpr' :: TypePhase -> * where
  LApp :: App tc -> LExpr' tc
  LSelect :: Select tc -> LExpr' tc
  LName :: Name -> LExpr' tc

deriving instance Eq (LExpr' tc)
deriving instance Show (LExpr' tc)

type AppU = App 'UnTyped
type AppC = App 'Typed
data App :: TypePhase -> * where
  App :: Expr tc -> Args tc -> App tc

deriving instance Eq (App tc)
deriving instance Show (App tc)


type ArgsU = Args 'UnTyped
type ArgsC = Args 'Typed
data Args :: TypePhase -> * where
  Args :: Purity -> [Expr tc] -> Args tc

deriving instance Eq (Args tc)
deriving instance Show (Args tc)


data Select :: TypePhase -> * where
  Select :: Expr tc -> Name -> Select tc

deriving instance Eq (Select tc)
deriving instance Show (Select tc)

type Name = String
type Names = [Name]


data Kind
  = KNamespace
  | KType
  | KExpr TypeT

type TypeU = Type 'UnTyped
type TypeT = Type 'Typed
data Type :: TypePhase -> * where
  TUser :: Typename -> Type tp

  -- Neither caller nor callee care about left-most mutability of param and return types
  TFunc :: Purity -> [Type tp] -> Type tp -> Type tp

  TTempRef :: Mut -> Type tp -> Type tp
  TPersRef :: Mut -> Type tp -> Type tp

  TOption :: Mut -> Type tp -> Type tp
  TZeroPlus :: Mut -> Type tp -> Type tp
  TOnePlus :: Mut -> Type tp -> Type tp

  TBln :: Type tp
  TChr :: Type tp
  TFlt :: Type tp
  TInt :: Type tp
  TNat :: Type tp
  TStr :: Type tp

  TNone :: Type tp

  -- Type errors can only occur after the ast has been type checked
  TError :: Error -> TypeT

deriving instance Eq (Type tp)
deriving instance Ord (Type tp)
deriving instance Show (Type tp)

type Typename = String

data Purity
  = Pure
  | PRead
  | PWrite
  deriving(Eq, Ord, Show)

data Mut
  = Mut         -- Mutable in present scope
  | Imut        -- Immutable in present scope
  -- Constant   -- Not mutable in any scope - planned
  -- CtConstant -- Known at compile time - planned
  deriving(Eq, Ord, Show)


type Errors = [Error]

data Error

  = UnknownId String

  | UnknownTypeName String
  | AmbiguousTypeName String

  -- Better conflict or mismatch?
  | TypeConflict { typeRequired :: TypeT, typeFound :: TypeT }

  -- Incompatible types? No common supertype?
  | FailedToUnify (Set.Set TypeT)

  | NonApplicable TypeT
  | WrongPurity { purityRequired :: Purity, purityFound :: Purity }
  | WrongNumArgs { numArgsRequired :: Int, numArgsFound :: Int }

  | UndefinedOperator BinOp TypeT TypeT

  -- Multiple, competing, duplicate, overlapping, contrandictory?...
  | CompetingDefinitions

  | RecursiveDefinition (Set.Set Name)

  | NeedExprFoundType
  | NeedExprFoundNamespace

  | Propagated

  deriving(Eq, Ord, Show)

recursiveDefinition :: [String] -> Error
recursiveDefinition = RecursiveDefinition . Set.fromList


