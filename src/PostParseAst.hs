{-# language GADTs #-}

module PostParseAst (postParseAst) where

import Control.Monad.Writer

import Ast
import Ast.Error
import Util.MultiMap

postParseAst :: Ast0 -> (Ast1, Errors)
postParseAst = runWriter . mapUnits

type ErrorM a = Writer Errors a

raise :: Error -> ErrorM ()
raise e = tell [e]

mapNamedList :: (a -> ErrorM b) -> [Named a] -> ErrorM (Table b)
mapNamedList f xs = do
  let mapNamed (Named n x) = f x >>= \x' -> return (n, x')
  tuples <- mapM mapNamed xs
  return $ multiFromList tuples

mapUnits :: [Named Unit0] -> ErrorM (Table Unit1)
mapUnits = mapNamedList mapUnit

mapUnit :: Unit0 -> ErrorM Unit1
mapUnit unit = case unit of
  UNamespace0 us     -> mapUnits us >>= return . UNamespace1
  UClass (Class0 ms) -> mapMembers ms >>= return . UClass . Class1
  UFunc f            -> mapFunc f >>= return . UFunc
  UVar v             -> mapVar v >>= return . UVar

mapMembers :: [Named Member0] -> ErrorM (Table Member1)
mapMembers = mapNamedList mapMember

mapMember :: Member0 -> ErrorM Member1
mapMember member = case member of
  MClass acc (Class0 ms) -> mapMembers ms >>= return . (MClass acc) . Class1
  MCons acc f            -> mapFunc f >>= return . MCons acc
  MFunc acc mut f        -> mapFunc f >>= return . MFunc acc mut
  MVar acc v             -> mapVar v >>= return . MVar acc

mapFunc :: Func0 -> ErrorM Func1
mapFunc (Func0 sig retNotation block) = mapBlock retNotation block >>= return . Func1 sig

mapBlock :: RetNotation -> Block0 -> ErrorM Block1
mapBlock retNotation (Block0 stmts) = do
  (Block1 stmts' ret) <- mapBlock' retNotation stmts []
  return $ Block1 (reverse stmts') ret

  where
    mapBlock' :: RetNotation -> [Stmt0] -> [Stmt1] -> ErrorM Block1

    mapBlock' ImplicitRet [SExpr e] xs = mapExpr e >>= return . Block1 xs . Just

    mapBlock' _ [SRet e] xs = mapExpr e >>= return . Block1 xs . Just

    mapBlock' ImplicitRet [s] xs = do
      raise ImplicitRetWithoutFinalExpr
      s' <- mapStmt s
      return $ Block1 (s' : xs) $ Nothing

    mapBlock' _ (SRet e : _) xs = do
      raise MidBlockReturnStatement
      mapExpr e >>= return . Block1 xs . Just

    mapBlock' rn (SExpr (Expr0 (EApp app)) : rest) xs = do
      app' <- mapApp app
      mapBlock' rn rest (SApp app' : xs)

    mapBlock' rn (SExpr _ : rest) xs = do
      raise UselessExpression
      mapBlock' rn rest xs

    mapBlock' rn (s : rest) xs = do
      s' <- mapStmt s
      mapBlock' rn rest $ s' : xs

    mapBlock' _ _ _ = error "All cases should have been accounted for"

    mapStmt :: Stmt0 -> ErrorM Stmt1
    mapStmt s = case s of

      SVar (Named n v) -> mapVar v >>= return . SVar . Named n
      SFunc (Named n f) -> mapFunc f >>= return . SFunc . Named n

      SExpr _ -> error "Should have been already handled"
      SRet _ -> error "Should have been already handled"


mapVar :: Var0 -> ErrorM Var1
mapVar (Var0 mut typ expr) = mapExpr expr >>= return . Var0 mut typ

mapExpr :: Expr0 -> ErrorM Expr1
mapExpr (Expr0 e) = mapExpr' e >>= return . Expr0

mapExpr' :: Expr0' -> ErrorM Expr1'
mapExpr' expr = case expr of

  EApp app -> mapApp app >>= return . EApp

  EName n -> return $ EName n

  EIf e1 ec e2 -> do
    e1' <- mapExpr e1
    ec' <- mapExpr ec
    e2' <- mapExpr e2
    return $ EIf e1' ec' e2'

  EUnOp op e -> do
    e' <- mapExpr e
    return $ EUnOp op e'

  EBinOp op e1 e2 -> do
    e1' <- mapExpr e1
    e2' <- mapExpr e2
    return $ EBinOp op e1' e2'

  EVal v -> return $ EVal v

mapApp :: App0 -> ErrorM App1
mapApp (App e (Args purity args)) = do
    e' <- mapExpr e
    args' <- mapM mapExpr args
    return $ App e' $ Args purity args'

