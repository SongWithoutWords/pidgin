module TypeCheck.SubAst
  ( subAst
  ) where

import Control.Monad.Writer
import Data.Map as M

import Ast.A2Constrained as A2
import qualified Ast.A3Typed as A3
import TypeCheck.ErrorM
import TypeCheck.Substitution
import Util.MultiMap
import Util.Preface

subAst :: Substitutions -> Ast -> (A3.Ast, Errors)
subAst = runErrorM .: subAst'

subAst' :: Substitutions -> Ast -> ErrorM A3.Ast
subAst' substitutions ast = multiMapM subUnit ast

  where
    subUnit :: Unit -> ErrorM A3.Unit
    subUnit unit = case unit of
      UNamespace units -> A3.UNamespace <$> multiMapM subUnit units
      UFunc f -> A3.UFunc <$> subFunc f
      UVar v -> A3.UVar <$> subVar v

    subVar :: Var -> ErrorM A3.Var
    subVar (Var t e) = liftM2 A3.Var (subType' t) (subExpr e)

    subFunc :: Func -> ErrorM A3.Func
    subFunc (Func s b) = liftM2 A3.Func (subSig s) (subBlock b)

    subSig :: Sig -> ErrorM A3.Sig
    subSig (Sig pur params retType) =
      liftM2 (A3.Sig pur) (subParams params) (subType' retType)

    subParams :: Params -> ErrorM A3.Params
    subParams ps = mapM subParam ps

    subParam :: Param -> ErrorM A3.Param
    subParam (Named n t) = (Named n) <$> subType' t

    subBlock :: Block -> ErrorM A3.Block
    subBlock (Block stmts optExpr) =
      liftM2 A3.Block (mapM subStmt stmts) (mapM subExpr optExpr)

    subStmt :: Stmt -> ErrorM A3.Stmt
    subStmt stmt = case stmt of
      SAssign lexpr expr -> liftM2 A3.SAssign (subLExpr lexpr) (subExpr expr)
      SVar namedVar -> A3.SVar <$> mapM subVar namedVar
      SApp app -> A3.SApp <$> subApp app

    subExpr :: Expr -> ErrorM A3.Expr
    subExpr (Expr t e) = liftM2 A3.Expr (subType' t) (subExpr' e)

    subExpr' :: Expr' -> ErrorM A3.Expr'
    subExpr' expr = let subExp = subExpr in case expr of
      EApp app -> A3.EApp <$> subApp app
      ESelect sel -> A3.ESelect <$> subSelect sel
      EName n -> pure $ A3.EName n
      EIf (Cond ec) e1 e2 ->
        liftM3 A3.EIf (A3.Cond <$> subExp ec) (subExp e1) (subExp e2)
      ELambda f -> A3.ELambda <$> subFunc f
      EUnOp op e1 -> (A3.EUnOp op) <$> (subExp e1)
      EBinOp op e1 e2 -> liftM2 (A3.EBinOp op) (subExp e1) (subExp e2)
      EVal v -> pure $ A3.EVal v

    subLExpr :: LExpr -> ErrorM A3.LExpr
    subLExpr (LExpr t l) = liftM2 A3.LExpr (subType' t) (subLExpr' l)

    subLExpr' :: LExpr' -> ErrorM A3.LExpr'
    subLExpr' l = case l of
      LApp app -> A3.LApp <$> subApp app
      LSelect sel -> A3.LSelect <$> subSelect sel
      LName n -> pure $ A3.LName n

    subApp :: App -> ErrorM A3.App
    subApp (App e args) = liftM2 A3.App (subExpr e) (subArgs args)

    subArgs :: Args -> ErrorM A3.Args
    subArgs (Args purity exprs) = (A3.Args purity) <$> (mapM subExpr exprs)

    subSelect :: Select -> ErrorM A3.Select
    subSelect (Select e name) = subExpr e >>= \e' -> pure $ A3.Select e' name

    subType' :: Type -> ErrorM A3.Type
    subType' typ = case typ of
      TFunc p paramTypes retType ->
        liftM2 (A3.TFunc p) (mapM subType' paramTypes) (subType' retType)
      TRef t -> A3.TRef <$> subType' t
      TVar tvar -> subTypeVar tvar
      t -> pure t
      where
        subTypeVar :: TVar -> ErrorM A3.Type
        subTypeVar tvar = case M.lookup tvar substitutions of
          Nothing -> raise (FailedToInferType typ) >> pure typ
          Just t@(TVar _) -> raise (FailedToInferType t) >> pure t
          Just t -> pure t

