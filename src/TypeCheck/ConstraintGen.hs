{-# language GADTs #-}

module TypeCheck.ConstraintGen
  ( constrainAst
  , module TypeCheck.Constraint
  ) where


import Ast
import Preface

import TypeCheck.Constraint
import TypeCheck.ConstrainM

import MultiMap

constrainAst :: Ast1 -> (Ast2, [Constraint])
constrainAst ast =

  -- tie the knot, in order to refer to typevars further ahead in the input
  let result@(ast', _) = runConstrainM (checkAst ast) ast'
  in result


checkAst :: Ast1 -> ConstrainM Ast2
checkAst = multiMapM checkUnit

checkUnit :: Unit1 -> ConstrainM Unit2
checkUnit = undefined

checkNamedExpr :: Named Expr1 -> ConstrainM (Named Expr2)
checkNamedExpr = traverse checkExpr

checkExpr :: Expr1 -> ConstrainM Expr2
checkExpr (Expr0 expression) = case expression of

  EName name -> do
    kinds <- lookupKinds name
    let
      t = case kinds of
        [] -> TError $ UnknownId name
        [KExpr t] -> case t of
          TError _ -> TError Propagated
          _ -> t
        [KType] -> TError NeedExprFoundType
        [KNamespace] -> TError NeedExprFoundNamespace
        _ -> TError CompetingDefinitions

    pure $ Expr2 t $ EName name


  ELambda (Func1 (Sig0 pur params optRetType) (Block1 stmts optRetExpr)) -> do
    tLam <- getNextTypeVar
    tRet <- getNextTypeVar

    optRetType' <- traverse checkType optRetType
    traverse (constrain tRet) optRetType'

    params' <- mapM (\(Param m t n) -> checkType t >>= (\t' -> pure $ Param m t' n)) params

    pushNewScope

    mapM (\(Param _ t n) -> pushLocal n t) params'

    optRetExpr' <- traverse checkExpr optRetExpr

    popScope

    let tRetExpr = case optRetExpr' of Nothing -> TNone; Just (Expr2 t _) -> t
    constrain tRet tRetExpr

    let tParams = (\(Param m t _) -> t) <$> params'
    constrain tLam $ TFunc pur tParams tRet

    pure $ Expr2 tLam $ ELambda $ Func1 (Sig2 pur params' tRet) (Block1 [] optRetExpr')


  EApp (App expr (Args purity args)) -> do
    tRet <- getNextTypeVar

    expr'@(Expr2 t1 _) <- checkExpr expr
    args' <- traverse checkExpr args

    let argTypes = (\(Expr2 t _) -> t) <$> args'

    constrain t1 $ TFunc purity argTypes tRet

    pure $ Expr2 tRet $ EApp $ App expr' (Args purity args')

  EIf e1 e2 e3 -> do
    tIf <- getNextTypeVar

    e1'@(Expr2 t1 _) <- checkExpr e1
    e2'@(Expr2 t2 _) <- checkExpr e2
    e3'@(Expr2 t3 _) <- checkExpr e3

    constrain TBln t1
    constrain tIf t2
    constrain tIf t3

    pure $ Expr2 tIf $ EIf e1' e2' e3'

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

  EVal v -> case v of
    b@VBln{} -> pure $ Expr2 TBln $ EVal b
    i@VInt{} -> pure $ Expr2 TInt $ EVal i


checkType :: Type0 -> ConstrainM Type2
checkType typ =
  let
    checkAndRet f m t = do
      t' <- checkType t
      return $ f m t'

  in case typ of
  TUser typeName -> error "Handle this case!"
    -- do
    -- bindings <- getBindings

    -- case lookupKinds bindings typeName of
    --   [] -> foundError $ UnknownTypeName typeName
    --   [KType] -> return $ TUser typeName
    --   _ -> foundError $ AmbiguousTypeName typeName

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

