module X.TypeCheck.ConstraintGen
  ( constrainAst
  -- , module TypeCheck.Constraint
  ) where

import Control.Monad(liftM2)
-- import Data.Maybe(mapMaybe)

import Ast.Common.Name
import qualified Ast.A1PostParse as A1
-- import qualified X.Ast as A2
import X.Ast
-- import Ast.A2Constrained.Error
-- import Ast.Common.Name
-- import TypeCheck.Constraint
import X.TypeCheck.ConstrainM
import X.TypeCheck.Util
import Util.MultiMap

constrainAst :: A1.Ast -> Ast
constrainAst ast =
  -- tie the knot, in order to refer to typevars further ahead in the input
  let
    (namespace, state) = runST $ runConstrainM (checkUnits ast) ast'
    ast' = constrainStateToAst namespace state
  in ast'

checkUnits :: A1.Ast -> ConstrainM s Namespace
checkUnits = multiMapWithKeyM checkUnit

checkUnit :: Name -> A1.Unit -> ConstrainM s Unit
checkUnit name unit = case unit of
  A1.UNamespace n -> UNamespace <$> checkUnits n

  -- A1.UData members -> UData <$> multiMapM checkMember members
  -- A1.UFunc f -> UFunc <$> (checkFunc name f >>= addFunc)
  A1.UFunc f -> UFunc <$> addFunc (checkFunc name f)
    -- fId <- getNextFId
    -- func <- (checkFunc name f fId)
    -- addFunc func
    -- pure $ UFunc fId
  -- A1.UVar v -> UVar <$> (checkVar name v >>= addVar)

checkMember :: A1.Member -> ConstrainM s Member
checkMember = undefined
-- checkMember (A1.MData acc members) = MData acc <$> multiMapM checkMember members
-- checkMember (A1.MVar acc typ) = MVar acc <$> checkType typ

-- checkFunc :: Name -> A1.Func -> ConstrainM s FuncId
-- checkFunc = addFunc (checkFunc')
-- (A1.Func (A1.Sig pur params optRetType) block) = do

checkFunc :: Name -> A1.Func -> FuncId -> ConstrainM s Func
checkFunc name (A1.Func (A1.Sig pur params optRetType) block) fId = do

  pushSearchNode $ UFunc fId

  tRet <- case optRetType of
    Just t -> checkType t
    Nothing -> getNextTypeVar

  params' <- mapM (\(A1.Param m t n) -> (,) n . (applyMut m) <$> checkType t) params

  pushNewScope

  mapM_ addLocalBinding params'

  block' <- checkFuncBlock tRet block

  popScope

  popSearchNode

  pure $ Func name (Sig pur params' tRet) block'

checkFuncBlock :: Type -> A1.Block -> ConstrainM s Block
checkFuncBlock _ [] = pure []
checkFuncBlock t [e] = do
  e' <- checkExpr e
  pure [Expr t $ ECons e']

checkFuncBlock t (e:es) = liftM2 (:) (checkExpr e) (checkFuncBlock t es)

checkBlock :: A1.Block -> ConstrainM s Block
checkBlock block = mapM checkExpr block

checkVar' :: A1.Var -> ConstrainM s Expr
checkVar' (A1.Var mut optType expr) = do
  expr' <- checkExpr expr
  optType' <- traverse checkType optType

  pure $ case optType' of
    Just t -> Expr (applyMut mut t) $ ECons expr'
    Nothing -> expr'

checkExpr :: A1.Expr -> ConstrainM s Expr
checkExpr expression = case expression of

  A1.EIf cond b1 b2 -> do

    cond' <- checkExpr cond

    -- I think these blocks may leak local references due to lack of pushScope
    b1' <- checkBlock b1
    b2' <- checkBlock b2

    pure $ Expr TUnknown $ EIf cond' b1' b2'

  -- A1.EVar (n, expr) -> do
  --   expr' <- checkVar expr
  --   addLocalBinding $ (n, typeOfExpr expr')
  --   pure $ Expr TNone $ EVar n expr'

  A1.ESelect expr name -> do
    expr' <- checkExpr expr
    case expr' of
      ENamespace units -> do
        exprs <- mapM exprOfUnit $ multiLookup name units
        case exprs of

          -- TODO: I need a way to report this error only if it's the best overload
          [] -> pure $ Expr TError EBinding
          [e] -> pure $ e
          es -> pure $ EOver es

    -- units <- lookupKinds name
    -- t <- getNextTypeVar
    -- pure $ Expr t $ ESelect expr' name kinds

  -- A1.EName name -> do
  --   kinds <- lookupKinds name
  --   t <- getNextTypeVar
  --   pure $ Expr t $ EName name kinds

  -- A1.ELambda f -> do
  --   f' <- checkFunc f
  --   pure $ Expr (typeOfFunc f') $ ELambda f'

  A1.EApp expr purity args -> do
    tRet <- getNextTypeVar

    expr' <- checkExpr expr
    args' <- traverse checkExpr args

    pure $ Expr tRet $ EApp purity expr' args'

  A1.EVal v -> pure $ Expr t $ EVal v
    where
      t = case v of
        A1.VBln _ -> TBln
        A1.VChr _ -> TChr
        A1.VFlt _ -> TFlt
        A1.VInt _ -> TInt
        A1.VStr _ -> TStr

-- checkExpr' :: A1.Expr -> ConstrainM s Expr'
-- checkExpr' expression = case expression of
--   A1.ESelect expr name -> do
--     expr' <- checkExpr' expr

--     case expr' of

--       Eselect e n ->

--       ESelect (EName [UNamespace units]) n ->
--         pure $ Expr t $ EName $ lookupUnits n units
      -- _ -> 


