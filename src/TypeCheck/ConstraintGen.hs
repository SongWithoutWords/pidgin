module TypeCheck.ConstraintGen
  ( constrainAst
  , module TypeCheck.Constraint
  ) where

import Control.Monad(when)
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
import Util.Preface


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
  block'@(A2.Block _ optRetExpr') <- checkBlock block
  let tRetExpr = case optRetExpr' of
        Nothing -> TNone
        Just (A2.Expr t _) -> t
  tRet $= tRetExpr

  popScope

  pure $ A2.Func (A2.Sig pur params' tRet) block'

checkBlock :: A1.Block -> ConstrainM A2.Block
checkBlock (A1.Block stmts maybeRetExpr) = do
  stmts' <- traverse checkStmt stmts
  maybeRetExpr' <- traverse checkExpr maybeRetExpr
  return $ A2.Block stmts' maybeRetExpr'

checkStmt :: A1.Stmt -> ConstrainM A2.Stmt
checkStmt stmt = case stmt of

  -- TODO: will need to account for mutations in future
  A1.SAssign lhs rhs -> do
    lhs'@(A2.Expr tLhs _) <- checkExpr lhs

    case tLhs of
      TMut _ -> pure ()
      _ -> raise AssignmentToImmutableValue

    rhs'@(A2.Expr tRhs _) <- checkExpr rhs
    tLhs $= tRhs
    pure $ A2.SAssign lhs' rhs'

  A1.SVar (Named name var) -> do
    var' <- checkVar var
    addLocalBinding $ Named name $ typeOfVar var'
    pure $ A2.SVar $ Named name var'

  A1.SIf ifBranch -> undefined

  A1.SExpr e -> A2.SExpr <$> checkExpr e


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
        KType -> Nothing
        KExpr e -> Just e
  let foundError err = (raise err) >> (pure $ A2.Expr TError $ A2.EName name)

  kinds <- lookupKinds name
  case kinds of
    [] -> foundError $ UnknownId name
    [KNamespace] -> foundError NeedExprFoundNamespace
    [KType] -> foundError NeedExprFoundType
    ks -> case mapMaybe exprOfKind ks of
      [] -> foundError (NoExpressionWithName name)
      [e] -> pure e
      es -> do
        let ts = map typeOfExpr es
        t <- (flip TOver ts) <$> getNextTVar
        pure $ A2.Expr t $ A2.EOver es

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


  A1.EIf (A1.Cond cond) e1 e2 -> do

    cond'@(A2.Expr tCond _) <- checkExpr cond
    e1'@(A2.Expr t1 _) <- checkExpr e1
    e2'@(A2.Expr t2 _) <- checkExpr e2

    TBln $= tCond
    t1 $= t2

    pure $ A2.Expr t1 $ A2.EIf (A2.Cond cond') e1' e2'


  A1.EVal v -> pure $ A2.Expr t $ A2.EVal v
    where
      t = case v of
        A1.VBln _ -> TBln
        A1.VChr _ -> TChr
        A1.VFlt _ -> TFlt
        A1.VInt _ -> TInt
        A1.VStr _ -> TStr

applyMut :: Mut -> Type -> Type
applyMut Mut = TMut
applyMut _ = identity

checkType :: A1.Type -> ConstrainM A2.Type
checkType typ = case  typ of
  A1.TUser typeName -> do
    kinds <- lookupKinds typeName
    case kinds of
      [] -> raise (UnknownTypeName typeName) >> pure TError
      [KType] -> return $ TUser typeName
      _ -> raise (AmbiguousTypeName typeName) >> pure TError

  A1.TFunc purity params ret -> do
    params' <- mapM checkType params
    ret' <- checkType ret
    return $ TFunc purity params' ret'

  A1.TRef m t -> A2.TRef . (applyMut m) <$> checkType t

  A1.TBln -> return TBln
  A1.TChr -> return TChr
  A1.TFlt -> return TFlt
  A1.TInt -> return TInt
  A1.TNat -> return TNat
  A1.TStr -> return TStr

  A1.TNone -> return TNone

