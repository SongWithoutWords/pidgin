{-# language GADTs #-}

module TypeCheck.ConstraintGen
  ( constrainAst
  , module TypeCheck.Constraint
  ) where

import Ast
import Ast.Error
import Preface
import TypeCheck.Constraint
import TypeCheck.ConstrainM
import TypeCheck.Util
import Util.MultiMap


constrainAst :: Ast1 -> (Ast2, [Constraint], Errors)
constrainAst ast =
  -- tie the knot, in order to refer to typevars further ahead in the input
  let result@(ast', _, _) = runConstrainM (checkUnits ast) ast'
  in result


checkUnits :: Ast1 -> ConstrainM Ast2
checkUnits = multiMapM checkUnit

checkUnit :: Unit1 -> ConstrainM Unit2
checkUnit unit = case unit of
  UNamespace1 n -> UNamespace1 <$> checkUnits n
  UFunc f -> UFunc <$> checkFunc f
  UVar v -> UVar <$> checkVar v

checkFunc :: Func1 -> ConstrainM Func2
checkFunc (Func1 (Sig0 pur params optRetType) block) = do
  tRet <- getNextTypeVar

  optRetType' <- traverse checkType optRetType
  traverse (constrain tRet) optRetType'

  params' <- mapM (\(Param m t n) -> checkType t >>= (\t' -> pure $ Param m t' n)) params
  pushNewScope

  mapM (\(Param _ t n) -> addLocalBinding n t) params'
  block'@(Block1 _ optRetExpr') <- checkBlock block
  let tRetExpr = case optRetExpr' of Nothing -> TNone; Just (Expr2 t _) -> t
  constrain tRet tRetExpr

  popScope

  pure $ Func1 (Sig2 pur params' tRet) block'

checkBlock :: Block1 -> ConstrainM Block2
checkBlock (Block1 stmts maybeRetExpr) = do
  stmts' <- traverse checkStmt stmts
  maybeRetExpr' <- traverse checkExpr maybeRetExpr
  return $ Block1 stmts' maybeRetExpr'

checkStmt :: Stmt1 -> ConstrainM Stmt2
checkStmt stmt = case stmt of

  -- TODO: will need to account for mutations in future
  SAssign lexpr expr -> undefined

  SVar (Named name var) -> do
    var' <- checkVar var
    addLocalBinding name $ typeOfVar var'
    pure $ SVar $ Named name var'

  SFunc f -> undefined

  SIf ifBranch -> undefined


checkVar :: Var1 -> ConstrainM Var2
checkVar (Var0 mut optType expr) = do
  expr' <- checkExpr expr
  optType' <- traverse checkType optType

  tVar <- case optType' of
    Just t -> pure t
    Nothing -> getNextTypeVar

  constrain tVar $ typeOfExpr expr'
  return $ Var2 mut tVar expr'

checkNamedExpr :: Named Expr1 -> ConstrainM (Named Expr2)
checkNamedExpr = traverse checkExpr

checkExpr :: Expr1 -> ConstrainM Expr2
checkExpr (Expr0 expression) = case expression of

  EName name -> do
    kinds <- lookupKinds name
    t <- case kinds of
      [] -> foundError $ UnknownId name
      [KExpr t] -> pure t
      [KType] -> foundError NeedExprFoundType
      [KNamespace] -> foundError NeedExprFoundNamespace
      _ -> foundError CompetingDefinitions
    pure $ Expr2 t $ EName name


  ELambda f -> do
    tLam <- getNextTypeVar
    f' <- checkFunc f
    constrain tLam $ typeOfFunc f'
    pure $ Expr2 tLam $ ELambda f'


  EApp (App expr (Args purity args)) -> do
    tRet <- getNextTypeVar

    expr'@(Expr2 t1 _) <- checkExpr expr
    args' <- traverse checkExpr args

    let argTypes = (\(Expr2 t _) -> t) <$> args'

    constrain t1 $ TFunc purity argTypes tRet

    pure $ Expr2 tRet $ EApp $ App expr' (Args purity args')

  EIf e1 e2 e3 -> do

    e1'@(Expr2 t1 _) <- checkExpr e1
    e2'@(Expr2 t2 _) <- checkExpr e2
    e3'@(Expr2 t3 _) <- checkExpr e3

    constrain TBln t2
    constrain t1 t3

    pure $ Expr2 t1 $ EIf e1' e2' e3'

  EUnOp op e -> let
    checkUnOp :: UnOp -> Type2 -> Type2 -> ConstrainM ()
    checkUnOp Neg tExpr tRes = do
      mapM_ (constrain TInt) [tExpr, tRes]
    in do
      e'@(Expr2 t _) <- checkExpr e
      tRes <- getNextTypeVar
      checkUnOp op t tRes
      pure $ Expr2 tRes $ EUnOp op e'

  EBinOp op e1 e2 -> let

    checkBinOp :: BinOp -> Type2 -> Type2 -> Type2 -> ConstrainM ()

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
      e1'@(Expr2 t1 _) <- checkExpr e1
      e2'@(Expr2 t2 _) <- checkExpr e2

      tRes <- getNextTypeVar
      checkBinOp op t1 t2 tRes

      pure $ Expr2 tRes $ EBinOp op e1' e2'

  EVal v -> pure $ Expr2 t $ EVal v
    where
      t = case v of
        VBln _ -> TBln
        VChr _ -> TChr
        VFlt _ -> TFlt
        VInt _ -> TInt
        VStr _ -> TStr

checkType :: Type0 -> ConstrainM Type2
checkType typ =
  let
    checkAndRet f m t = do
      t' <- checkType t
      return $ f m t'

  in case typ of
  TUser typeName -> do
    kinds <- lookupKinds typeName
    case kinds of
      [] -> foundError $ UnknownTypeName typeName
      [KType] -> return $ TUser typeName
      _ -> foundError $ AmbiguousTypeName typeName

  TFunc purity params ret -> do
    params' <- mapM checkType params
    ret' <- checkType ret
    return $ TFunc purity params' ret'

  TTempRef m t -> checkAndRet TTempRef m t
  TPersRef m t -> checkAndRet TPersRef m t

  TOption m t -> checkAndRet TOption m t
  TZeroPlus m t -> checkAndRet TZeroPlus m t
  TOnePlus m t -> checkAndRet TOnePlus m t

  TBln -> return TBln
  TChr -> return TChr
  TFlt -> return TFlt
  TInt -> return TInt
  TNat -> return TNat
  TStr -> return TStr

  TNone -> return TNone

