module TypeCheck.ConstraintGen
  ( constrainAst
  , module TypeCheck.Constraint
  ) where

import qualified Ast.A1PostParse as A1
import qualified Ast.A2Constrained as A2
import Ast.A2Constrained.Error
import Ast.Common.Name
import Ast.Common.Op
import TypeCheck.Constraint
import TypeCheck.ConstrainM
import TypeCheck.Kind
import TypeCheck.Util
import Util.MultiMap
import Util.Preface


constrainAst :: A1.Ast -> (A2.Ast, [Constraint], Errors)
constrainAst ast =
  -- tie the knot, in order to refer to typevars further ahead in the input
  let result@(ast', _, _) = runConstrainM (checkUnits ast) ast'
  in result

type Constrain a b = a -> ConstrainM b

checkUnits :: Constrain A1.Ast A2.Ast
checkUnits = multiMapM checkUnit

checkUnit :: Constrain A1.Unit A2.Unit
checkUnit unit = case unit of
  A1.UNamespace n -> A2.UNamespace <$> checkUnits n
  A1.UFunc f -> A2.UFunc <$> checkFunc f
  A1.UVar v -> A2.UVar <$> checkVar v

checkFunc :: Constrain A1.Func A2.Func
checkFunc (A1.Func (A1.Sig pur params optRetType) block) = do
  tRet <- getNextTypeVar

  optRetType' <- traverse checkType optRetType
  traverse (constrain tRet) optRetType'

  params' <- mapM (\(A1.Param m t n) -> checkType t >>= (\t' -> pure $ A2.Param m t' n)) params
  pushNewScope

  mapM (\(A2.Param _ t n) -> addLocalBinding n t) params'
  block'@(A2.Block _ optRetExpr') <- checkBlock block
  let tRetExpr = case optRetExpr' of Nothing -> TNone; Just (A2.Expr t _) -> t
  constrain tRet tRetExpr

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
  A1.SAssign lexpr expr -> undefined

  A1.SVar (Named name var) -> do
    var' <- checkVar var
    addLocalBinding name $ typeOfVar var'
    pure $ A2.SVar $ Named name var'

  A1.SFunc f -> undefined

  A1.SIf ifBranch -> undefined


checkVar :: A1.Var -> ConstrainM A2.Var
checkVar (A1.Var mut optType expr) = do
  expr' <- checkExpr expr
  optType' <- traverse checkType optType

  tVar <- case optType' of
    Just t -> pure t
    Nothing -> getNextTypeVar

  constrain tVar $ typeOfExpr expr'
  return $ A2.Var mut tVar expr'

checkNamedExpr :: Named A1.Expr -> ConstrainM (Named A2.Expr)
checkNamedExpr = traverse checkExpr

checkExpr :: A1.Expr -> ConstrainM A2.Expr
checkExpr expression = case expression of

  A1.EName name -> do
    kinds <- lookupKinds name
    t <- case kinds of
      [] -> foundError $ UnknownId name
      [KExpr t] -> pure t
      [KType] -> foundError NeedExprFoundType
      [KNamespace] -> foundError NeedExprFoundNamespace
      _ -> foundError CompetingDefinitions
    pure $ A2.Expr t $ A2.EName name


  A1.ELambda f -> do
    tLam <- getNextTypeVar
    f' <- checkFunc f
    constrain tLam $ typeOfFunc f'
    pure $ A2.Expr tLam $ A2.ELambda f'


  A1.EApp (A1.App expr (A1.Args purity args)) -> do
    tRet <- getNextTypeVar

    expr'@(A2.Expr t1 _) <- checkExpr expr
    args' <- traverse checkExpr args

    let argTypes = (\(A2.Expr t _) -> t) <$> args'

    constrain t1 $ TFunc purity argTypes tRet

    pure $ A2.Expr tRet $ A2.EApp $ A2.App expr' (A2.Args purity args')

  A1.EIf (A1.Cond cond) e1 e2 -> do

    cond'@(A2.Expr tCond _) <- checkExpr cond
    e1'@(A2.Expr t1 _) <- checkExpr e1
    e2'@(A2.Expr t2 _) <- checkExpr e2

    constrain TBln tCond
    constrain t1 t2

    pure $ A2.Expr t1 $ A2.EIf (A2.Cond cond') e1' e2'

  A1.EUnOp op e -> let
    checkUnOp :: UnOp -> A2.Type -> A2.Type -> ConstrainM ()
    checkUnOp Neg tExpr tRes = do
      mapM_ (constrain TInt) [tExpr, tRes]
    in do
      e'@(A2.Expr t _) <- checkExpr e
      tRes <- getNextTypeVar
      checkUnOp op t tRes
      pure $ A2.Expr tRes $ A2.EUnOp op e'

  A1.EBinOp op e1 e2 -> let

    checkBinOp :: BinOp -> A2.Type -> A2.Type -> A2.Type -> ConstrainM ()

    -- Int -> Int -> Int
    checkBinOp Add a b r = do
      mapM_ (constrain TInt) [a, b, r]

    checkBinOp Sub a b r = do
      mapM_ (constrain TInt) [a, b, r]

    checkBinOp Mul a b r = do
      mapM_ (constrain TInt) [a, b, r]

    checkBinOp Div a b r = do
      mapM_ (constrain TInt) [a, b, r]

    checkBinOp Mod a b r = do
      mapM_ (constrain TInt) [a, b, r]

    -- Int -> Int -> Bln
    checkBinOp (Cmp _) a b r = do
      mapM_ (constrain TInt) [a, b]
      constrain TBln r

    -- Bln -> Bln -> Bln
    checkBinOp And a b r = do
      mapM_ (constrain TBln) [a, b, r]

    checkBinOp Or a b r = do
      mapM_ (constrain TBln) [a, b, r]

    in do
      e1'@(A2.Expr t1 _) <- checkExpr e1
      e2'@(A2.Expr t2 _) <- checkExpr e2

      tRes <- getNextTypeVar
      checkBinOp op t1 t2 tRes

      pure $ A2.Expr tRes $ A2.EBinOp op e1' e2'

  A1.EVal v -> pure $ A2.Expr t $ A2.EVal v
    where
      t = case v of
        A1.VBln _ -> TBln
        A1.VChr _ -> TChr
        A1.VFlt _ -> TFlt
        A1.VInt _ -> TInt
        A1.VStr _ -> TStr

checkType :: A1.Type -> ConstrainM A2.Type
checkType typ =
  let
    checkAndRet f m t = checkType t >>= pure f m
      -- do
      -- t' <- checkType t
      -- return $ f m t'

  in case typ of
  A1.TUser typeName -> do
    kinds <- lookupKinds typeName
    case kinds of
      [] -> foundError $ UnknownTypeName typeName
      [KType] -> return $ TUser typeName
      _ -> foundError $ AmbiguousTypeName typeName

  A1.TFunc purity params ret -> do
    params' <- mapM checkType params
    ret' <- checkType ret
    return $ TFunc purity params' ret'

  A1.TRef m t -> checkType t >>= pure . A2.TRef m-- checkAndRet TTempRef m t
  -- TPersRef m t -> checkAndRet TPersRef m t

  -- TOption m t -> checkAndRet TOption m t
  -- TZeroPlus m t -> checkAndRet TZeroPlus m t
  -- TOnePlus m t -> checkAndRet TOnePlus m t

  A1.TBln -> return TBln
  A1.TChr -> return TChr
  A1.TFlt -> return TFlt
  A1.TInt -> return TInt
  A1.TNat -> return TNat
  A1.TStr -> return TStr

  A1.TNone -> return TNone

