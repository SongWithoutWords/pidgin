module PostParse(postParse) where

import Data.Monoid()
import Control.Monad(liftM2)
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
  A0.UData (A0.Data ms)   -> A1.UData . A1.Data <$> mapMembers ms
  A0.UFunc f              -> A1.UFunc <$> mapFunc f
  A0.UVar v               -> A1.UVar <$> mapVar v

mapMembers :: [Named A0.Member] -> ErrorM (Table A1.Member)
mapMembers = tableFromNamedM mapMember

mapMember :: A0.Member -> ErrorM A1.Member
mapMember member = case member of
  A0.MData acc (A0.Data ms)   -> A1.MData acc . A1.Data <$> mapMembers ms
  A0.MVar acc v               -> A1.MVar acc <$> mapVar v

mapFunc :: A0.Func -> ErrorM A1.Func
mapFunc (A0.Func sig block) = A1.Func sig <$> mapBlock block

listOf :: a -> [a]
listOf a = [a]

mapBlock :: A0.Block -> ErrorM A1.Block -- -> ErrorM A1.Block

mapBlock [] = pure []

-- mapBlock (A0.ERet)

mapBlock (A0.ERet e : (_:_)) = raise MidBlockReturn >> listOf . A1.ERet <$> mapExpr e

mapBlock (e : rest) = liftM2 (:) (mapExpr e) (mapBlock rest)

-- No statements left => no ret value
-- mapBlock [] rn xs = do
  -- when (rn == A0.ImplicitRet) $ raise ImplicitRetWithoutFinalExpr
  -- return $ A1.Block xs Nothing

-- One remaining expr + implicit ret => ret value
-- mapBlock [e] xs =
--   A1.Block xs . Just <$> mapExpr e

-- mapBlock (s : rest) rn xs = case s of

--   A0.SExpr expr -> case expr of
--     -- Application can be significant without being returned
--     e@A0.EApp{} -> do
--       e' <- mapExpr e
--       mapBlock rest rn (A1.SExpr e' : xs)
--     -- All other expressions are insignificant when not returned
--     _ -> do
--       raise UselessExpression
--       mapBlock rest rn xs

--   -- Ret statement => ret value
--   A0.ERet expr -> do
--     -- Raise error/warning if mid-block
--     when (not $ null rest) $ raise MidBlockReturnStatement
--     A1.Block xs . Just <$> mapExpr expr

--   A0.EVar var -> do
--     var' <- mapM mapVar var
--     mapBlock rest rn (A1.SVar var' : xs)

--   -- TODO: enforce proper assignment, syntax sugar for a(b) = c
--   A0.EAssign e1 e2 -> do
--     e1' <- mapExpr e1
--     e2' <- mapExpr e2
--     mapBlock rest rn (A1.SAssign e1' e2' : xs)

--   A0.EIf i -> do
--     i' <- mapIfBranch i
--     mapBlock rest A0.ExplicitRet (A1.SIf i' : xs)

mapVar :: A0.Var -> ErrorM A1.Var
mapVar (A0.Var mut typ expr) = mapExpr expr >>= return . A1.Var mut typ

mapExpr :: A0.Expr -> ErrorM A1.Expr
mapExpr expr = case expr of

  A0.EIf cond b1 b2 -> do
    cond' <- mapExpr cond
    b1' <- mapBlock b1
    b2' <- mapBlock b2
    return $ A1.EIf cond' b1' b2'

  A0.EVar (Named n var) -> A1.EVar . Named n <$> mapVar var
  A0.EAssign e1 e2 -> liftM2 A1.EAssign (mapExpr e1) (mapExpr e2)

  A0.EApp e purity args -> do
    e' <- mapExpr e
    args' <- mapM mapExpr args
    return $ A1.EApp e' purity args'

  A0.EName n -> return $ A1.EName n


  A0.EVal v -> return $ A1.EVal v

