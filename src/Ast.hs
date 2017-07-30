{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}

-- Potential future edit: parameterize using actual collection and an inferrable type
-- (may be able to reuse more code across data constructors)

module Ast
  ( module Ast
  , module Types
  ) where

import MultiMap
import Types
import TypeErrors

-- Finite number of steps friend!
data Storage
  = SList
  | SMap

data TypePhase
  = TpChecked
  | TpUnchecked


type Table a = MultiMap Name a

type AstLu = [UnitLu]
type AstMu = Table UnitMu
type AstMc = Table UnitMc


type UnitLu = Unit 'SList 'TpUnchecked
type UnitMu = Unit 'SMap 'TpUnchecked
type UnitMc = Unit 'SMap 'TpChecked
data Unit :: Storage -> TypePhase -> * where

  UNamespaceL :: Name -> [Unit 'SList ts] -> Unit 'SList ts
  UNamespaceM :: Table (Unit 'SMap ts) -> Unit 'SMap ts

  UClass :: Class col ts -> Unit col ts

  UFuncL :: Func ts -> Unit 'SList ts
  UFuncM :: Lambda ts -> Unit 'SMap ts

  UVar :: Var col ts -> Unit col ts

deriving instance Eq (Unit col tc)
deriving instance Show (Unit col tc)


type ClassLu = Class 'SList 'TpUnchecked
type ClassMu = Class 'SMap 'TpUnchecked
type ClassMc = Class 'SMap 'TpChecked
data Class :: Storage -> TypePhase -> * where
  ClassL :: Name -> [Member 'SList tc] -> Class 'SList tc
  ClassM :: Table (Member 'SMap tc) -> Class 'SMap tc

deriving instance Eq (Class col tc)
deriving instance Show (Class col tc)


type MemberLu = Member 'SList 'TpUnchecked
type MemberMu = Member 'SMap 'TpUnchecked
type MemberMc = Member 'SMap 'TpChecked
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


type LambdaU = Lambda 'TpUnchecked
type LambdaC = Lambda 'TpChecked
data Lambda :: TypePhase -> * where
  Lambda :: Sig tc -> Block tc -> Lambda tc

deriving instance Eq (Lambda tc)
deriving instance Show (Lambda tc)


type SigU = Sig 'TpUnchecked
type SigC = Sig 'TpChecked
data Sig :: TypePhase -> * where
  SigU :: Purity -> Params -> Maybe Type -> Sig 'TpUnchecked
  SigC :: Purity -> Params -> TypeOrErrors -> Sig 'TpChecked

deriving instance Eq (Sig tc)
deriving instance Show (Sig tc)


type Params = [Param]

data Param
  = Param Mut Type Name
  deriving(Eq, Show)

type BlockU = Block 'TpUnchecked
type BlockC = Block 'TpChecked
type Block tc = [Stmt tc]

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


type VarLu = Var 'SList 'TpUnchecked
type VarMu = Var 'SMap 'TpUnchecked
type VarLc = Var 'SList 'TpChecked
type VarMc = Var 'SMap 'TpChecked
data Var :: Storage -> TypePhase -> * where
  VarLu :: Mut -> Maybe Type -> Name -> ExprU -> VarLu
  VarMu :: Mut -> Maybe Type -> ExprU -> VarMu
  VarLc :: Mut -> TypeOrErrors -> Name -> ExprC -> VarLc
  VarMc :: Mut -> TypeOrErrors -> ExprC -> VarMc

deriving instance Eq (Var f c)
deriving instance Show (Var f c)

type ExprU = Expr 'TpUnchecked
type ExprC = Expr 'TpChecked
data Expr :: TypePhase -> * where
  EApp :: App tc -> Expr tc
  ESelect :: Select tc -> Expr tc
  EName :: Name -> Expr tc

  EIf :: Expr tc -> {- if -} Expr tc -> {- else -} Expr tc -> Expr tc
  ELambda :: Lambda tc -> Expr tc
  ECons :: Typename -> Args tc -> Expr tc

  -- Unary operators
  ENegate :: Expr tc -> Expr tc

  -- Binary operators
  EAdd :: Expr tc -> Expr tc -> Expr tc
  ESub :: Expr tc -> Expr tc -> Expr tc
  EMul :: Expr tc -> Expr tc -> Expr tc
  EDiv :: Expr tc -> Expr tc -> Expr tc
  EGreater :: Expr tc -> Expr tc -> Expr tc
  ELesser :: Expr tc -> Expr tc -> Expr tc
  EGreaterEq :: Expr tc -> Expr tc -> Expr tc
  ELesserEq :: Expr tc -> Expr tc -> Expr tc

  -- Literals
  ELitBln :: Bool -> Expr tc
  ELitChr :: Char -> Expr tc
  ELitFlt :: Float -> Expr tc
  ELitInt :: Int -> Expr tc
  ELitStr :: String -> Expr tc

deriving instance Eq (Expr tc)
deriving instance Show (Expr tc)


-- Expressions that can appear on the left side of an assignment
data LExpr :: TypePhase -> * where
  LApp :: App tc -> LExpr tc
  LSelect :: Select tc -> LExpr tc
  LName :: Name -> LExpr tc

deriving instance Eq (LExpr tc)
deriving instance Show (LExpr tc)

type AppU = App 'TpUnchecked
type AppC = App 'TpChecked
data App :: TypePhase -> * where
  App :: Expr tc -> Args tc -> App tc

deriving instance Eq (App tc)
deriving instance Show (App tc)

type ArgsU = Args 'TpUnchecked
type ArgsC = Args 'TpChecked
data Args :: TypePhase -> * where
  Args :: Purity -> [Expr tc] -> Args tc

deriving instance Eq (Args tc)
deriving instance Show (Args tc)


data Select :: TypePhase -> * where
  Select :: Expr tc -> Name -> Select tc

deriving instance Eq (Select tc)
deriving instance Show (Select tc)


type Name = String

