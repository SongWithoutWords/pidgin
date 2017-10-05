{-# language GADTs #-}

module TypeCheck.SubAst
  ( subAst
  ) where

import Control.Monad.Reader

import Ast
import MultiMap
import TypeCheck.Substitution

type Sub a = Substitutions -> a -> a

subAst :: Sub Ast2
subAst s = multiMap (subUnit s)

subUnit :: Sub Unit2
subUnit s unit = case unit of
  UNamespace1 units -> UNamespace1 $ subAst s units
  UFunc f -> UFunc $ subFunc s f
  UVar v -> UVar $ subVar s v

subVar :: Sub Var2
subVar s (Var2 m t e) = Var2 m (subType s t) e

subFunc :: Sub Func2
subFunc s (Func1 sig b) = Func1 (subSig s sig) (subBlock s b)

subSig :: Sub Sig2
subSig s (Sig2 pur params retType) = Sig2 pur (subParams s params) retType

subParams :: Sub Params2
subParams s ps = map subParam ps
  where subParam (Param m t n) = Param m (subType s t) n

subBlock :: Sub Block2
subBlock s (Block1 stmts optExpr) =
  Block1 (subStmt s <$> stmts) (subExpr s <$> optExpr)

subStmt :: Sub Stmt2
subStmt s stmt = case stmt of
  SAssign lexpr expr -> SAssign (subLExpr s lexpr) (subExpr s expr)
  SVar namedVar -> SVar $ subVar s <$> namedVar
  SFunc namedFunc -> SFunc $ subFunc s <$> namedFunc
  SApp app -> SApp $ subApp s app

subExpr :: Sub Expr2
subExpr s (Expr2 t e) = Expr2 (subType s t) (subExpr' s e)

subExpr' :: Sub Expr2'
subExpr' s expr = let subExp = subExpr s in case expr of
  EApp app -> EApp $ subApp s app
  ESelect sel -> ESelect $ subSelect s sel
  EName n -> EName n
  EIf e1 e2 e3 -> EIf (subExp e1) (subExp e2) (subExp e3)
  ELambda f -> ELambda $ subFunc s f
  EUnOp op e1 -> EUnOp op (subExp e1)
  EBinOp op e1 e2 -> EBinOp op (subExp e1) (subExp e2)
  EVal v -> EVal v

subLExpr :: Sub LExpr2
subLExpr s (LExpr2 t l) = LExpr2 (subType s t) (subLExpr' s l)

subLExpr' :: Sub LExpr2'
subLExpr' s l = case l of
  LApp app -> LApp $ subApp s app
  LSelect sel -> LSelect $ subSelect s sel
  LName n -> LName n

subApp :: Sub App2
subApp s (App e args) = App (subExpr s e) (subArgs s args)

subArgs :: Sub Args2
subArgs s (Args purity exprs) = Args purity (subExpr s <$> exprs)

subSelect :: Sub Select2
subSelect s (Select e name) = Select (subExpr s e) name

