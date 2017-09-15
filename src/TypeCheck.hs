{-# language GADTs #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RecursiveDo #-}

module TypeCheck(typeCheckAst) where

-- import Data.Maybe

import Preface
import Control.Monad

import Ast
import Ast2Builder
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
    UFunc l -> UFunc <$> checkLambda l
    UVar v -> UVar <$> checkVar v
  popSearchName
  return res


checkLambda :: Func1 -> TypeCheckM s Func2
checkLambda (Func1 (Sig0 purity params maybeRetType) block) = do

  params' <- checkParams params

  maybeRetType' <- checkOptionalType maybeRetType

  bindings <- getBindings

  let blockBindings = initBlockBindings bindings params'

  block' <- withBindings blockBindings $ checkBlock block

  -- TODO: enforce/infer based on types returned
  let typesReturnedUnified = undefined -- unifyTypes typesReturned

  returnType <- enforceOrInfer maybeRetType' typesReturnedUnified
  return $ Func1 (Sig2 purity params' returnType) block'

-- Yields a type checked block and a list of returned types
checkBlock :: Block1 -> TypeCheckM s Block2 -- [Stmt1] -> TypeCheckM s ([Stmt2], [Type2])

checkBlock = undefined -- TODO: Come up with a good way of doing this

-- checkBlock [] _ = return ([], [])

-- checkBlock [SExpr e] ImplicitRet = do
--   e' <- checkExpr e
--   return ([SExpr e'], [typeOfExpr e'])

-- checkBlock (stmt:stmts) retStyle = do
--   (stmt', maybeRet) <- checkStmt stmt
--   (stmts', rets) <- checkBlock stmts retStyle
--   return (stmt':stmts', maybeRet ?: rets)

checkStmt :: Stmt1 -> TypeCheckM s (Stmt2, Maybe Type2)
checkStmt stmt = case stmt of

  -- TODO: will need to account for mutations in future
  SAssign lexpr expr -> undefined

  SVar (Named name (Var0 mut typ expr)) -> do
    expr' <- checkExpr expr
    typ' <- checkOptionalType typ

    varType <- enforceOrInfer typ' $ typeOfExpr expr'
    modifyBindings $ addLocalBinding name $ KExpr $ typeOfExpr expr'
    return (SVar $ Named name $ Var2 mut varType expr', Nothing)

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
checkOptionalType opt = case opt of
      Just t -> do
        t' <- checkType t
        return $ Just t'
      _ -> return $ Nothing

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


  EBinOp operator e1 e2 ->
    let
      opIsScalar :: BinOp -> Bool
      opIsScalar And = False
      opIsScalar Or = False
      opIsScalar _ = True

      opIsBoolean = not . opIsScalar

      checkSymmetricalOp isDefined op typ a b =
        if isDefined op
        then return $ e2BinOp typ op a b
        else do
          let err = UndefinedOperator op typ typ
          raise err
          return $ e2BinOp (TError err) op a b


      checkBinOp :: BinOp -> Expr2 -> Expr2 -> TypeCheckM s Expr2

      checkBinOp op a@(Expr2 (TError _) _) b =
        return $ e2BinOp (TError Propagated) op a b

      checkBinOp op a b@(Expr2 (TError _) _) =
        return $ e2BinOp (TError Propagated) op a b

      checkBinOp op a@(Expr2 TFlt _) b@(Expr2 TFlt _) =
        checkSymmetricalOp opIsScalar op TFlt a b

      checkBinOp op a@(Expr2 TInt _) b@(Expr2 TInt _) =
        checkSymmetricalOp opIsScalar op TInt a b

    in do
      e1' <- checkExpr e1
      e2' <- checkExpr e2
      checkBinOp operator e1' e2'


  EValBln b -> return $ e2ValBln b
  EValFlt f -> return $ e2ValFlt f
  EValInt i -> return $ e2ValInt i
  EValStr s -> return $ e2ValStr s


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
            RecursiveDefinition cycle -> do
              history <- getHistory
              let sourceName = head history
              if elem sourceName cycle
                then return $ TError $ RecursiveDefinition cycle
                else return $ TError Propagated
            _ -> return $ TError Propagated

          t -> return t

        [KType] -> foundError NeedExprFoundType

        [KNamespace] -> foundError NeedExprFoundNamespace

        _ -> foundError CompetingDefinitions

