module TypeCheck.ConstraintGen
  ( constrainAst
  , module TypeCheck.Constraint
  ) where

import Control.Monad(liftM2)
import Data.Maybe(mapMaybe)

import qualified Ast.A1PostParse as A1
import qualified Ast.A2Constrained as A2
import Ast.A2Constrained.Error
import Ast.Common.Name
import TypeCheck.Constraint
import TypeCheck.ConstrainM
import TypeCheck.Kind
import TypeCheck.Util
import Util.MultiMap


constrainAst :: A1.Ast -> (A2.Ast, [Constraint], Errors)
constrainAst ast =
  -- tie the knot, in order to refer to typevars further ahead in the input
  let (ast', constraints, errors) = runConstrainM (checkUnits ast) ast'
  -- reverse the constraints so that they appear in the order generated
  in (ast', reverse constraints, errors)

checkUnits :: A1.Ast -> ConstrainM A2.Ast
checkUnits = multiMapM checkUnit

checkUnit :: A1.Unit -> ConstrainM A2.Unit
checkUnit unit = case unit of
  A1.UNamespace n -> A2.UNamespace <$> checkUnits n
  A1.UFunc f -> A2.UFunc <$> checkFunc f
  A1.UVar v -> A2.UVar <$> checkVar v

checkFunc :: A1.Func -> ConstrainM A2.Func
checkFunc (A1.Func (A1.Sig pur params optRetType) block) = do

  tRet <- case optRetType of
    Just t -> checkType t
    Nothing -> getNextTypeVar

  params' <- mapM (\(A1.Param m t n) -> (Named n) . (applyMut m) <$> checkType t) params

  pushNewScope

  mapM_ addLocalBinding params'
  block' <- checkBlock block

  let tRetExpr = typeOfExpr $ last block' -- TODO: use a more robust function than last

  tRet $= tRetExpr

  popScope

  pure $ A2.Func (A2.Sig pur params' tRet) block'

checkBlock :: A1.Block -> ConstrainM A2.Block
checkBlock block = mapM checkExpr block

-- checkStmt :: A1.Stmt -> ConstrainM A2.Stmt
-- checkStmt stmt = case stmt of

--   -- TODO: will need to account for mutations in future
--   A1.SAssign lhs rhs -> do
--     lhs'@(A2.Expr tLhs _) <- checkExpr lhs

--     case tLhs of
--       TMut _ -> pure ()
--       _ -> raise AssignmentToImmutableValue

--     rhs'@(A2.Expr tRhs _) <- checkExpr rhs
--     tLhs $= tRhs
--     pure $ A2.SAssign lhs' rhs'

--   A1.SVar (Named name var) -> do
--     var' <- checkVar var
--     addLocalBinding $ Named name $ typeOfVar var'
--     pure $ A2.SVar $ Named name var'

--   A1.SIf ifBranch -> A2.SIf <$> checkIf ifBranch

--   A1.SExpr e -> A2.SExpr <$> checkExpr e

-- checkIf :: A1.IfBranch -> ConstrainM A2.IfBranch
-- checkIf i = case i of
--   A1.If cb -> A2.If <$> checkCondBlock cb
--   A1.IfElse cb b -> liftM2 A2.IfElse (checkCondBlock cb) (checkBlock b)

-- checkCondBlock :: A1.CondBlock -> ConstrainM A2.CondBlock
-- checkCondBlock (A1.CondBlock e b) = liftM2 A2.CondBlock (checkExpr e) (checkBlock b)

checkVar :: A1.Var -> ConstrainM A2.Var
checkVar (A1.Var mut optType expr) = do
  expr' <- checkExpr expr
  optType' <- traverse checkType optType

  tVar <- case optType' of
    Just t -> pure t
    Nothing -> getNextTypeVar

  tVar $= (typeOfExpr expr')
  return $ A2.Var (applyMut mut tVar) expr'


checkName :: Name -> ConstrainM A2.Expr
checkName name = do
  let exprOfKind k = case k of
        KNamespace -> Nothing
        KType _ -> Nothing
        KExpr e -> Just e
  let foundError err = (raise err) >> (pure $ A2.Expr TError $ A2.EName name)

  kinds <- lookupKinds name
  case kinds of
    [] -> foundError $ UnknownId name
    [KNamespace] -> foundError NeedExprFoundNamespace
    [KType _] -> foundError NeedExprFoundType
    ks -> case mapMaybe exprOfKind ks of
      [] -> foundError (NoExpressionWithName name)
      [e] -> pure e
      es -> do
        let ts = map typeOfExpr es
        t <- (flip TOver ts) <$> getNextTVar
        pure $ A2.Expr t $ A2.EOver es

-- checkExprs :: A1.Exprs -> ConstrainM A2.Expr
-- checkExprs exprs = last <$> mapM checkExpr exprs

checkExpr :: A1.Expr -> ConstrainM A2.Expr
checkExpr expression = case expression of

  A1.EName name -> checkName name

  A1.ELambda f -> do
    f' <- checkFunc f
    pure $ A2.Expr (typeOfFunc f') $ A2.ELambda f'


  A1.EApp expr purity args -> do
    tRet <- getNextTypeVar

    expr'@(A2.Expr tExpr _) <- checkExpr expr
    args' <- traverse checkExpr args

    let argTypes = typeOfExpr <$> args'

    TFunc purity argTypes tRet $= tExpr

    pure $ A2.Expr tRet $ A2.EApp expr' purity args'


  A1.EIf cond b1 b2 -> do

    cond'@(A2.Expr tCond _) <- checkExpr cond

    b1' <- checkBlock b1
    b2' <- checkBlock b2

    let t1 = typeOfExpr $ last b1'
    let t2 = typeOfExpr $ last b2'

    TBln $= tCond
    t1 $= t2

    pure $ A2.Expr t1 $ A2.EIf cond' b1' b2'


  A1.EVal v -> pure $ A2.Expr t $ A2.EVal v
    where
      t = case v of
        A1.VBln _ -> TBln
        A1.VChr _ -> TChr
        A1.VFlt _ -> TFlt
        A1.VInt _ -> TInt
        A1.VStr _ -> TStr



