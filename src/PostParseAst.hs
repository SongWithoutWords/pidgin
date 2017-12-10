{-# language GADTs #-}

module PostParseAst (postParseAst) where

import Control.Monad.Writer

import qualified Ast.A0Parse as A0
import qualified Ast.A1PostParse as A1
import Ast.A2Constrained.Error
import Ast.Common.Name
import Ast.Common.Table

postParseAst :: A0.Ast -> (A1.Ast, Errors)
postParseAst = runWriter . mapUnits

type ErrorM a = Writer Errors a

raise :: Error -> ErrorM ()
raise e = tell [e]

mapUnits :: [Named A0.Unit] -> ErrorM (Table A1.Unit)
mapUnits = tableFromNamedM mapUnit

mapUnit :: A0.Unit -> ErrorM A1.Unit
mapUnit unit = case unit of
  A0.UNamespace us     -> mapUnits us >>= return . A1.UNamespace
  A0.UClass (A0.Class ms) -> mapMembers ms >>= return . A1.UClass . A1.Class
  A0.UFunc f            -> mapFunc f >>= return . A1.UFunc
  A0.UVar v             -> mapVar v >>= return . A1.UVar

mapMembers :: [Named A0.Member] -> ErrorM (Table A1.Member)
mapMembers = tableFromNamedM mapMember

mapMember :: A0.Member -> ErrorM A1.Member
mapMember member = case member of
  A0.MClass acc (A0.Class ms) -> mapMembers ms >>= return . (A1.MClass acc) . A1.Class
  A0.MCons acc f            -> mapFunc f >>= return . A1.MCons acc
  A0.MFunc acc mut f        -> mapFunc f >>= return . A1.MFunc acc mut
  A0.MVar acc v             -> mapVar v >>= return . A1.MVar acc

mapFunc :: A0.Func -> ErrorM A1.Func
mapFunc (A0.Func sig retNotation block) = mapBlock retNotation block >>= return . A1.Func sig

mapBlock :: A0.RetNotation -> A0.Block -> ErrorM A1.Block
mapBlock retNotation stmts = do
  (A1.Block stmts' ret) <- mapBlock' retNotation stmts []
  return $ A1.Block (reverse stmts') ret

  where
    mapBlock' :: A0.RetNotation -> [A0.Stmt] -> [A1.Stmt] -> ErrorM A1.Block

    mapBlock' A0.ImplicitRet [A0.SExpr e] xs = mapExpr e >>= return . A1.Block xs . Just

    mapBlock' _ [A0.SRet e] xs = mapExpr e >>= return . A1.Block xs . Just

    mapBlock' A0.ImplicitRet [s] xs = do
      raise ImplicitRetWithoutFinalExpr
      s' <- mapStmt s
      return $ A1.Block (s' : xs) $ Nothing

    mapBlock' _ (A0.SRet e : _) xs = do
      raise MidBlockReturnStatement
      mapExpr e >>= return . A1.Block xs . Just

    mapBlock' rn (A0.SExpr (A0.EApp app) : rest) xs = do
      app' <- mapApp app
      mapBlock' rn rest (A1.SApp app' : xs)

    mapBlock' rn (A0.SExpr _ : rest) xs = do
      raise UselessExpression
      mapBlock' rn rest xs

    mapBlock' rn (s : rest) xs = do
      s' <- mapStmt s
      mapBlock' rn rest $ s' : xs

    mapBlock' _ _ _ = error "All cases should have been accounted for"

    mapStmt :: A0.Stmt -> ErrorM A1.Stmt
    mapStmt s = case s of

      A0.SVar (Named n v) -> mapVar v >>= return . A1.SVar . Named n
      A0.SFunc (Named n f) -> mapFunc f >>= return . A1.SFunc . Named n

      A0.SExpr _ -> error "Should have been already handled"
      A0.SRet _ -> error "Should have been already handled"


mapVar :: A0.Var -> ErrorM A1.Var
mapVar (A0.Var mut typ expr) = mapExpr expr >>= return . A1.Var mut typ

-- mapExpr :: A0.Expr -> ErrorM A1.Expr
-- mapExpr e = mapExpr' e >>= return . A1.Expr

mapExpr :: A0.Expr -> ErrorM A1.Expr
mapExpr expr = case expr of

  A0.EApp app -> mapApp app >>= return . A1.EApp

  A0.EName n -> return $ A1.EName n

  A0.EIf (A0.Cond cond) e1 e2 -> do
    cond' <- mapExpr cond
    e1' <- mapExpr e1
    e2' <- mapExpr e2
    return $ A1.EIf (A1.Cond cond') e1' e2'

  A0.EUnOp op e -> do
    e' <- mapExpr e
    return $ A1.EUnOp op e'

  A0.EBinOp op e1 e2 -> do
    e1' <- mapExpr e1
    e2' <- mapExpr e2
    return $ A1.EBinOp op e1' e2'

  A0.EVal v -> return $ A1.EVal v

mapApp :: A0.App -> ErrorM A1.App
mapApp (A0.App e (A0.Args purity args)) = do
    e' <- mapExpr e
    args' <- mapM mapExpr args
    return $ A1.App e' $ A1.Args purity args'

