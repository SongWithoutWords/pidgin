{-# language GADTs #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RecursiveDo #-}

module TypeCheck(typeCheckAst) where

import Preface
import Control.Monad

import Ast
import Ast2Builder
import Cycle
import Debug
import MultiMap
import TypeCheckM
import TypeCheckUtil


-- Knot tying implementation - it's awesome!
typeCheckAst :: Ast1 -> (Ast2, Errors)
typeCheckAst ast = trace "typeCheckAst" $ runST $ do
  history <- newSTRef []
  rec result@(typedAst, _) <- runWriterT $ evalStateT
        (checkUnitTable ast) $
        TypeContext (GlobalBindings typedAst) history
  return result

checkUnitTable :: Ast1 -> TypeCheckM s Ast2
checkUnitTable unitTable = trace "checkUnitTable" $
  multiMapWithKeyM (\name unit -> typeCheckLazy $ checkUnit name unit) unitTable

checkUnit :: Name -> Unit1 -> TypeCheckM s Unit2
checkUnit name unit = trace "checkUnit" $ do
  pushSearchName name
  res <- case unit of
    UNamespace1 n -> UNamespace1 <$> checkUnitTable n
    UFunc l -> UFunc <$> checkFunc l
    UVar v -> UVar <$> checkVar v
  popSearchName
  return res


checkFunc :: Func1 -> TypeCheckM s Func2
checkFunc (Func1 (Sig0 purity params maybeRetType) block) = do

  params' <- checkParams params

  maybeRetType' <- checkOptionalType maybeRetType

  bindings <- getBindings

  let blockBindings = initBlockBindings bindings params'

  block'@(Block1 _ maybeRetExpr) <- withBindings blockBindings $ checkBlock block

  -- TODO: Support returns from sub-blocks
  let typesReturnedUnified = case maybeRetExpr of Nothing -> TNone; Just e -> typeOfExpr e

  returnType <- enforceOrInfer maybeRetType' typesReturnedUnified
  return $ Func1 (Sig2 purity params' returnType) block'

checkBlock :: Block1 -> TypeCheckM s Block2

checkBlock (Block1 stmts maybeRetExpr) = do
  stmts' <- traverse checkStmt stmts
  maybeRetExpr' <- traverse checkExpr maybeRetExpr
  return $ Block1 stmts' maybeRetExpr'

checkStmt :: Stmt1 -> TypeCheckM s Stmt2
checkStmt stmt = case stmt of

  -- TODO: will need to account for mutations in future
  SAssign lexpr expr -> undefined

  SVar (Named name (Var0 mut typ expr)) -> do
    expr' <- checkExpr expr
    typ' <- checkOptionalType typ

    varType <- enforceOrInfer typ' $ typeOfExpr expr'
    modifyBindings $ addLocalBinding name $ KExpr $ typeOfExpr expr'
    return $ SVar $ Named name $ Var2 mut varType expr' -- , Nothing)

  SFunc f -> undefined

  SIf ifBranch -> undefined


-- TODO: Maintain a context within the block.

checkVar :: Var1 -> TypeCheckM s Var2
checkVar (Var0 mut typ expr) = do
  expr' <- checkExpr expr

  typ' <- checkOptionalType typ

  -- It's good that the Haskell type checker catches cases like this.
  varType <- enforceOrInfer typ' $ typeOfExpr expr'

  return $ Var2 mut varType expr'


checkType :: Type0 -> TypeCheckM s Type2
checkType typ =
  let
    checkAndRet f m t = do
      t' <- checkType t
      return $ f m t'

  in case typ of
  TUser typeName -> do
    bindings <- getBindings

    case lookupKinds bindings typeName of
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


checkOptionalType :: Maybe Type0 -> TypeCheckM s (Maybe Type2)
checkOptionalType = traverse checkType


checkParams :: Params0 -> TypeCheckM s Params2
checkParams = mapM checkParam

checkParam :: Param0 -> TypeCheckM s Param2
checkParam (Param m t n) = do
  t' <- checkType t
  return $ Param m t' n

checkExpr :: Expr1 -> TypeCheckM s Expr2
checkExpr (Expr0 expr) = trace "typeCheckExpr" $ case expr of

  EApp (App e args) -> trace "checkApp" $ do
    e' <- checkExpr e
    args' <- checkArgs args

    case typeOfExpr e' of
      TError _ -> return $ e2App (TError Propagated) e' args'

      TFunc purity paramTypes retType -> do
        let (Args argPurity argExprs) = args'
        let argTypes = map typeOfExpr argExprs

        when (purity /= argPurity) $ raise $ WrongPurity purity argPurity

        if length paramTypes /= length argTypes then
          raise $ WrongNumArgs (length paramTypes) (length argTypes)
        else
          zipWithM_ (<~) paramTypes argTypes

        return $ e2App retType e' args'

      _ -> do
        let err = NonApplicable $ typeOfExpr e'
        raise err
        return $ e2App (TError err) e' args'

  EName name -> do
    t <- checkName name
    return $ e2Name t name

  -- TODO: could I determine common root type, e.g. typeof(a if a.exists else none) == ?A
  EIf e1 ec e2 -> do
    e1' <- checkExpr e1
    e2' <- checkExpr e2
    ec' <- checkExpr ec

    typeOfExpr e1' <~ typeOfExpr e2'
    TBln <~ typeOfExpr ec'

    return $ e2If (typeOfExpr e1') e1' ec' e2'

  EUnOp operator e -> let
      checkUnOp :: UnOp -> Type2 -> TypeCheckM s Type2
      checkUnOp Neg TInt = pure TInt
    in do
      e'@(Expr2 t _) <- checkExpr e
      tRes <- checkUnOp operator t
      pure $ e2UnOp tRes operator e'

  EBinOp operator e1 e2 -> let

      checkBinOp :: BinOp -> Type2 -> Type2 -> TypeCheckM s Type2
      checkBinOp _ (TError _) _ = pure $ TError Propagated
      checkBinOp _ _ (TError _) = pure $ TError Propagated

      checkBinOp Add TInt TInt = pure TInt
      checkBinOp Sub TInt TInt = pure TInt
      checkBinOp Mul TInt TInt = pure TInt
      checkBinOp Div TInt TInt = pure TInt
      checkBinOp Mod TInt TInt = pure TInt

      checkBinOp (Cmp _) TInt TInt = pure TBln

    in do
      e1'@(Expr2 t1 _) <- checkExpr e1
      e2'@(Expr2 t2 _) <- checkExpr e2
      t <- checkBinOp operator t1 t2
      pure $ e2BinOp t operator e1' e2'

  EVal (VBln b) -> return $ e2ValBln b
  EVal (VFlt f) -> return $ e2ValFlt f
  EVal (VInt i) -> return $ e2ValInt i
  EVal (VStr s) -> return $ e2ValStr s


checkArgs :: Args1 -> TypeCheckM s Args2
checkArgs (Args purity exprs) = do
  exprs' <- mapM checkExpr exprs
  return $ Args purity exprs'

checkName :: Name -> TypeCheckM s Type2
checkName name = trace ("checkName " ++ name) $ do
  history <- getHistory
  traceM $ "searching for name " ++ name ++ " in " ++ show history
  if elem name history then do
    traceM $ "found " ++ name ++ " in " ++ show history ++
      "; raising recursive definition"
    let cycle = takeWhileInclusive (\x -> x /= name) history
    foundError $ recursiveDefinition cycle
  else do
    bindings <- getBindings
    findTypeName' $ lookupKinds bindings name
    where
      findTypeName' :: [Kind] -> TypeCheckM s Type2
      findTypeName' ks = trace "findTypeName'" $ case ks of
        [] -> do
          raise $ UnknownId name
          return $ TError $ UnknownId name
        [KExpr t] -> case t of

          TError err -> case err of
            RecursiveDefinition cycle@(Cycle names) -> do
              history <- getHistory
              let sourceName = head history
              if elem sourceName names
                then return $ TError $ RecursiveDefinition cycle
                else return $ TError Propagated
            _ -> return $ TError Propagated

          t -> return t

        [KType] -> foundError NeedExprFoundType

        [KNamespace] -> foundError NeedExprFoundNamespace

        _ -> foundError CompetingDefinitions

