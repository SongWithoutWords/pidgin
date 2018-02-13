module X.TypeCheck.ConstraintGen
  ( constrainAst
  -- , module TypeCheck.Constraint
  ) where

import Control.Monad(liftM2)
-- import Data.Maybe(mapMaybe)

import qualified Ast.A1PostParse as A1
import qualified X.Ast as A2
-- import Ast.A2Constrained.Error
-- import Ast.Common.Name
-- import TypeCheck.Constraint
import X.TypeCheck.ConstrainM
import X.TypeCheck.Util
import Util.MultiMap


constrainAst :: A1.Ast -> A2.Ast
constrainAst ast =
  -- tie the knot, in order to refer to typevars further ahead in the input
  let ast' = runConstrainM (checkUnits ast) ast'
  in ast'

checkUnits :: A1.Ast -> ConstrainM A2.Ast
checkUnits = multiMapM checkUnit

checkUnit :: A1.Unit -> ConstrainM A2.Unit
checkUnit unit = case unit of
  A1.UNamespace n -> A2.UNamespace <$> checkUnits n
  A1.UData members -> A2.UData <$> multiMapM checkMember members
  A1.UFunc f -> A2.UFunc <$> checkFunc f
  A1.UVar v -> A2.UVar <$> checkVar v

checkMember :: A1.Member -> ConstrainM A2.Member
checkMember (A1.MData acc members) = A2.MData acc <$> multiMapM checkMember members
checkMember (A1.MVar acc typ) = A2.MVar acc <$> checkType typ

checkFunc :: A1.Func -> ConstrainM A2.Func
checkFunc (A1.Func (A1.Sig pur params optRetType) block) = do

  tRet <- case optRetType of
    Just t -> checkType t
    Nothing -> getNextTypeVar

  params' <- mapM (\(A1.Param m t n) -> (,) n . (applyMut m) <$> checkType t) params

  pushNewScope

  mapM_ addLocalBinding params'

  block' <- checkFuncBlock tRet block

  popScope

  pure $ A2.Func (A2.Sig pur params' tRet) block'

checkFuncBlock :: A2.Type -> A1.Block -> ConstrainM A2.Block
checkFuncBlock _ [] = pure []
checkFuncBlock t [e] = do
  e' <- checkExpr e
  pure [A2.Expr t $ A2.ECons e']

checkFuncBlock t (e:es) = liftM2 (:) (checkExpr e) (checkFuncBlock t es)

checkBlock :: A1.Block -> ConstrainM A2.Block
checkBlock block = mapM checkExpr block

checkVar :: A1.Var -> ConstrainM A2.Expr
checkVar (A1.Var mut optType expr) = do
  expr' <- checkExpr expr
  optType' <- traverse checkType optType

  pure $ case optType' of
    Just t -> A2.Expr (applyMut mut t) $ A2.ECons expr'
    Nothing -> expr'

checkExpr :: A1.Expr -> ConstrainM A2.Expr
checkExpr expression = case expression of

  A1.EIf cond b1 b2 -> do

    cond' <- checkExpr cond

    b1' <- checkBlock b1
    b2' <- checkBlock b2

    -- I really need a better way of communicating that I haven't found a type just yet
    pure $ A2.Expr A2.TUnknown $ A2.EIf cond' b1' b2'

  A1.EVar (n, expr) -> do
    expr' <- checkVar expr
    addLocalBinding $ (n, typeOfExpr expr')
    pure $ A2.Expr A2.TNone $ A2.EVar n expr'

  A1.ESelect expr name -> do
    expr' <- checkExpr expr
    kinds <- lookupKinds name
    t <- getNextTypeVar
    pure $ A2.Expr t $ A2.ESelect expr' name kinds

  A1.EName name -> do
    kinds <- lookupKinds name
    t <- getNextTypeVar
    pure $ A2.Expr t $ A2.EName name kinds

  A1.ELambda f -> do
    f' <- checkFunc f
    pure $ A2.Expr (typeOfFunc f') $ A2.ELambda f'

  A1.EApp expr purity args -> do
    tRet <- getNextTypeVar

    expr' <- checkExpr expr
    args' <- traverse checkExpr args

    pure $ A2.Expr tRet $ A2.EApp purity expr' args'

  A1.EVal v -> pure $ A2.Expr t $ A2.EVal v
    where
      t = case v of
        A1.VBln _ -> A2.TBln
        A1.VChr _ -> A2.TChr
        A1.VFlt _ -> A2.TFlt
        A1.VInt _ -> A2.TInt
        A1.VStr _ -> A2.TStr

