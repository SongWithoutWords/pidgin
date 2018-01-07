module PostParse(postParse) where

import Data.Monoid()
import Control.Monad.Writer

import qualified Ast.A0Parse as A0
import qualified Ast.A1PostParse as A1
import Ast.A2Constrained.Error
import Ast.Common.Name
import Ast.Common.Table
import TypeCheck.ErrorM

postParse :: A0.Ast -> (A1.Ast, Errors)
postParse = runErrorM . mapUnits

mapUnits :: [Named A0.Unit] -> ErrorM (Table A1.Unit)
mapUnits = tableFromNamedM mapUnit

mapUnit :: A0.Unit -> ErrorM A1.Unit
mapUnit unit = case unit of
  A0.UNamespace us        -> A1.UNamespace <$> mapUnits us
  A0.UClass (A0.Class ms) -> (A1.UClass . A1.Class) <$> mapMembers ms
  A0.UFunc f              -> A1.UFunc <$> mapFunc f
  A0.UVar v               -> A1.UVar <$> mapVar v

mapMembers :: [Named A0.Member] -> ErrorM (Table A1.Member)
mapMembers = tableFromNamedM mapMember

mapMember :: A0.Member -> ErrorM A1.Member
mapMember member = case member of
  A0.MClass acc (A0.Class ms) -> (A1.MClass acc) . A1.Class <$> mapMembers ms
  A0.MCons acc f              -> A1.MCons acc <$> mapFunc f
  A0.MFunc acc mut f          -> A1.MFunc acc mut <$> mapFunc f
  A0.MVar acc v               -> A1.MVar acc <$> mapVar v

mapFunc :: A0.Func -> ErrorM A1.Func
mapFunc (A0.Func sig retNotation block) = A1.Func sig <$> mapBlock retNotation block

mapBlock :: A0.RetNotation -> A0.Block -> ErrorM A1.Block
mapBlock retNotation stmts = do
  (A1.Block stmts' ret) <- xMapBlock' stmts retNotation []
  return $ A1.Block (reverse stmts') ret

  where
    xMapBlock' :: [A0.Stmt] -> A0.RetNotation -> [A1.Stmt] -> ErrorM A1.Block

    -- No statements left => no ret value
    xMapBlock' [] rn xs = do
      when (rn == A0.ImplicitRet) $ raise ImplicitRetWithoutFinalExpr
      return $ A1.Block xs Nothing

    -- One remaining expr + implicit ret => ret value
    xMapBlock' [A0.SExpr e] A0.ImplicitRet xs =
      A1.Block xs . Just <$> mapExpr e

    xMapBlock' (s : rest) rn xs = case s of

      A0.SExpr expr -> case expr of
        -- Application can be significant without being returned
        A0.EApp app -> do
          app' <- mapApp app
          xMapBlock' rest rn (A1.SApp app' : xs)
        -- All other expressions are insignificant when not returned
        _ -> do
          raise UselessExpression
          xMapBlock' rest rn xs

      -- Ret statement => ret value
      A0.SRet expr -> do
        -- Raise error/warning if mid-block
        when (not $ null rest) $ raise MidBlockReturnStatement
        A1.Block xs . Just <$> mapExpr expr

      A0.SVar var -> do
        var' <- mapM mapVar var
        xMapBlock' rest rn (A1.SVar var' : xs)

      A0.SAssign lexpr expr -> do
        lexpr' <- mapLexpr lexpr
        expr' <- mapExpr expr
        xMapBlock' rest rn (A1.SAssign lexpr' expr' : xs)


mapVar :: A0.Var -> ErrorM A1.Var
mapVar (A0.Var mut typ expr) = mapExpr expr >>= return . A1.Var mut typ

mapLexpr :: A0.LExpr -> ErrorM A1.LExpr
mapLexpr lexpr = case lexpr of
  A0.LApp app -> A1.LApp <$> mapApp app
  A0.LName name -> pure $ A1.LName name

mapExpr :: A0.Expr -> ErrorM A1.Expr
mapExpr expr = case expr of

  A0.EApp app -> mapApp app >>= return . A1.EApp

  A0.EName n -> return $ A1.EName n

  A0.EIf (A0.Cond cond) e1 e2 -> do
    cond' <- mapExpr cond
    e1' <- mapExpr e1
    e2' <- mapExpr e2
    return $ A1.EIf (A1.Cond cond') e1' e2'

  A0.EVal v -> return $ A1.EVal v

mapApp :: A0.App -> ErrorM A1.App
mapApp (A0.App e args) = do
    e' <- mapExpr e
    args' <- mapArgs args
    return $ A1.App e' args'

mapArgs :: A0.Args -> ErrorM A1.Args
mapArgs (A0.Args purity exprs) = A1.Args purity <$> mapM mapExpr exprs

