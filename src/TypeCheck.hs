{-# language GADTs #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RecursiveDo #-}

module TypeCheck(typeCheckAst) where

-- import Data.Maybe

import Preface
import Control.Monad

import Ast
import AstBuilderT
import Debug
import MultiMap
import TypeCheckM
import TypeCheckUtil


-- Knot tying implementation - it's awesome!
typeCheckAst :: AstMu -> (AstMc, Errors)
typeCheckAst ast = trace "typeCheckAst" $ runST $ do
  history <- newSTRef []
  rec result@(typedAst, _) <- runWriterT $ evalStateT
        (checkUnitTable ast) $
        TypeContext (GlobalBindings typedAst) history
  return result

checkUnitTable :: AstMu -> TypeCheckM s AstMc
checkUnitTable unitTable = trace "checkUnitTable" $
  multiMapWithKeyM (\name unit -> typeCheckLazy $ checkUnit name unit) unitTable

checkUnit :: Name -> UnitMu -> TypeCheckM s UnitMc
checkUnit name unit = trace "checkUnit" $ do
  pushSearchName name
  res <- case unit of
    UNamespaceM n -> UNamespaceM <$> checkUnitTable n
    UFuncM l -> UFuncM <$> checkLambda l
    UVar v -> UVar <$> checkVar v
  popSearchName
  return res


checkLambda :: LambdaU -> TypeCheckM s LambdaC
checkLambda (Lambda (SigU purity params maybeRetType) returnStyle block) = do

  params' <- checkParams params

  maybeRetType' <- checkOptionalType maybeRetType

  bindings <- getBindings

  let blockBindings = initBlockBindings bindings params'

  (block', typesReturned) <- withBindings blockBindings $ checkBlock block returnStyle

  let typesReturnedUnified = unifyTypes typesReturned
  returnType <- enforceOrInfer maybeRetType' typesReturnedUnified
  return $ Lambda (SigC purity params' returnType) returnStyle block'

-- Yields a type checked block and a list of returned types
checkBlock :: [StmtU] -> RetNotation -> TypeCheckM s ([StmtC], [TypeT])

checkBlock [] _ = return ([], [])

checkBlock [SExpr e] ImplicitRet = do
  e' <- checkExpr e
  return ([SExpr e'], [typeOfExpr e'])

checkBlock (stmt:stmts) retStyle = do
  (stmt', maybeRet) <- checkStmt stmt
  (stmts', rets) <- checkBlock stmts retStyle
  return (stmt':stmts', maybeRet ?: rets)

checkStmt :: StmtU -> TypeCheckM s (StmtC, Maybe TypeT)
checkStmt stmt = case stmt of

  -- TODO: will need to account for mutations in future
  SAssign lexpr expr -> undefined

  SVar (VarLu mut typ name expr) -> do
    expr' <- checkExpr expr
    typ' <- checkOptionalType typ

    varType <- enforceOrInfer typ' $ typeOfExpr expr'
    modifyBindings $ addLocalBinding name $ KExpr $ typeOfExpr expr'
    return (SVar $ VarLc mut varType name expr', Nothing)

  SFunc f -> undefined

  SIf ifBranch -> undefined

  -- TODO: detect useless expressions
  SExpr e -> do
    e' <- checkExpr e
    -- (e', _) <- typeCheckExpr e
    return (SExpr e', Nothing)

  SRet e -> undefined



-- TODO: Maintain a context within the block.

checkVar :: VarMu -> TypeCheckM s VarMc
checkVar (VarMu mut typ expr) = do
  expr' <- checkExpr expr

  typ' <- checkOptionalType typ

  -- It's good that the Haskell type checker catches cases like this.
  varType <- enforceOrInfer typ' $ typeOfExpr expr'

  return $ VarMc mut varType expr'


checkType :: TypeU -> TypeCheckM s TypeT
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


checkOptionalType :: Maybe TypeU -> TypeCheckM s (Maybe TypeT)
checkOptionalType opt = case opt of
      Just t -> do
        t' <- checkType t
        return $ Just t'
      _ -> return $ Nothing

checkParams :: ParamsU -> TypeCheckM s ParamsT
checkParams = mapM checkParam

checkParam :: ParamU -> TypeCheckM s ParamT
checkParam (Param m t n) = do
  t' <- checkType t
  return $ Param m t' n

checkExpr :: ExprU -> TypeCheckM s ExprT
checkExpr (ExprU expr) = trace "typeCheckExpr" $ case expr of

  EApp (App e args) -> trace "checkApp" $ do
    e' <- checkExpr e
    args' <- checkArgs args

    case typeOfExpr e' of
      TError _ -> return $ tApp (TError Propagated) e' args'

      TFunc purity paramTypes retType -> do
        let (Args argPurity argExprs) = args'
        let argTypes = map typeOfExpr argExprs

        when (purity /= argPurity) $ raise $ WrongPurity purity argPurity

        if length paramTypes /= length argTypes then
          raise $ WrongNumArgs (length paramTypes) (length argTypes)
        else
          zipWithM_ (<~) paramTypes argTypes

        return $ tApp retType e' args'

      _ -> do
        let err = NonApplicable $ typeOfExpr e'
        raise err
        return $ tApp (TError err) e' args'

  EName name -> do
    t <- checkName name
    return $ tName t name

  -- TODO: could I determine common root type, e.g. typeof(a if a.exists else none) == ?A
  EIf e1 ec e2 -> do
    e1' <- checkExpr e1
    e2' <- checkExpr e2
    ec' <- checkExpr ec

    typeOfExpr e1' <~ typeOfExpr e2'
    TBln <~ typeOfExpr ec'

    return $ tIf (typeOfExpr e1') e1' ec' e2'


  EBinOp operator e1 e2 ->
    let
      opIsScalar :: BinOp -> Bool
      opIsScalar And = False
      opIsScalar Or = False
      opIsScalar _ = True

      opIsBoolean = not . opIsScalar

      checkSymmetricalOp isDefined op typ a b =
        if isDefined op
        then return $ tBinOp typ op a b
        else do
          let err = UndefinedOperator op typ typ
          raise err
          return $ tBinOp (TError err) op a b


      checkBinOp :: BinOp -> ExprT -> ExprT -> TypeCheckM s ExprT

      checkBinOp op a@(ExprT (TError _) _) b =
        return $ tBinOp (TError Propagated) op a b

      checkBinOp op a b@(ExprT (TError _) _) =
        return $ tBinOp (TError Propagated) op a b

      checkBinOp op a@(ExprT TFlt _) b@(ExprT TFlt _) =
        checkSymmetricalOp opIsScalar op TFlt a b

      checkBinOp op a@(ExprT TInt _) b@(ExprT TInt _) =
        checkSymmetricalOp opIsScalar op TInt a b

    in do
      e1' <- checkExpr e1
      e2' <- checkExpr e2
      checkBinOp operator e1' e2'


  EValBln b -> return $ tValBln b
  EValFlt f -> return $ tValFlt f
  EValInt i -> return $ tValInt i
  EValStr s -> return $ tValStr s


checkArgs :: ArgsU -> TypeCheckM s ArgsC
checkArgs (Args purity exprs) = do
  exprs' <- mapM checkExpr exprs
  return $ Args purity exprs'

checkName :: Name -> TypeCheckM s TypeT
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
      findTypeName' :: [Kind] -> TypeCheckM s TypeT
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

