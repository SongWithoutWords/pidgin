module TypeCheck.SubAst
  ( subAst
  ) where

import Control.Monad.Writer
import qualified Data.Map as M

import Ast.A2Constrained as A2
import qualified Ast.A3Typed as A3
import TypeCheck.ApplySubs
import TypeCheck.ErrorM
import TypeCheck.Substitution
import TypeCheck.Util
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
    subExpr (Expr typ expr) = do
      typ' <- subType' typ

      A3.Expr typ' <$> case expr of
        EApp app -> A3.EApp <$> subApp app
        ESelect sel -> A3.ESelect <$> subSelect sel
        EName n -> pure $ A3.EName n
        EIntr i -> pure $ EIntr i
        EIf (Cond ec) e1 e2 ->
          liftM3 A3.EIf (A3.Cond <$> subExpr ec) (subExpr e1) (subExpr e2)
        ELambda f -> A3.ELambda <$> subFunc f
        EVal v -> pure $ A3.EVal v
        EOver es -> do
          es' <- mapM subExpr es
          pure $ case filter ((==typ') . typeOfExpr) es' of
            [A3.Expr _ e] -> e
            es'' -> EOver es''


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
      x@(TVar tvar) -> case M.lookup tvar substitutions of
        Nothing -> raise (FailedToInferType x) >> pure x
        Just y -> pure y
      (TOver tvar ts) -> case M.lookup tvar substitutions of
        Nothing -> do
          x <- TOver tvar <$> mapM subType' ts
          raise (FailedToInferType x) >> pure x
        Just y -> pure y
      TFunc p paramTypes retType ->
        liftM2 (A3.TFunc p) (mapM subType' paramTypes) (subType' retType)
      TRef t -> A3.TRef <$> subType' t
      t -> pure t

