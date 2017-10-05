{-# language GADTs #-}

module TypeCheck.SubAst
  ( subAst
  ) where

import Control.Monad.Reader

import Ast
import MultiMap
import TypeCheck.Substitution

-- type SubM a = Reader Substitutions a

-- sub :: Type2 -> SubM Type2
-- sub = ask >>= subType
  -- env <- ask
  -- subType env


subAst :: Substitutions -> Ast2 -> Ast2
subAst s = multiMap (subUnit s)

subUnit :: Substitutions -> Unit2 -> Unit2
subUnit s unit = case unit of
  UNamespace1 units -> UNamespace1 $ subAst s units
  UFunc f -> UFunc $ subFunc s f
  UVar v -> UVar $ subVar v

subFunc :: Substitutions -> Func2 -> Func2
subFunc s (Func sig b) = Func (subSig s sig) (subBlock s b)

subExpr :: Substitutions -> Expr2 -> Expr2
subExpr s (Expr2 t e) = Expr2 (subType s t) (subExpr' s e)

subExpr' :: Substitutions -> Expr2' -> Expr2'
subExpr' s expr = let sub = subExpr s in case expr of
  EName n -> EName n
  ELambda (Named n t) e -> ELamT (Named n $ subType s t) $ sub e
  EApp e1 e2 -> EApp (sub e1) (sub e2)
  EIf e1 e2 e3 -> EIf sub e1 (sub e2) (sub e3)
  EBinOp op e1 e2 -> EBinOp op (sub e1) (sub e2)
  EVal v -> EVal v

