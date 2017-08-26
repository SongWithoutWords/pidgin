{-# language DataKinds #-}
{-# language GADTs #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RecursiveDo #-}

module TypeCheck(typeCheckAst) where

import Data.Maybe

import Ast
import Debug
import MultiMap
import TypeCheckM


-- Is type b assignable to type a?
class TypeCompare a b where
  (<~) :: a -> b -> TypeCheckM s ()

-- I think I can probably generalize the rhs to a functor. I think.
instance TypeCompare Type Type where
  a <~ b | (a /= b) = raise TypeConflict { expected = a, received = b }
         | otherwise = return ()

instance TypeCompare Type (Maybe Type) where
  _ <~ Nothing = return ()
  a <~ (Just b) = a <~ b

instance TypeCompare (Maybe Type) (Maybe Type) where
  Nothing <~ _ = return ()
  (Just a) <~ b = a <~ b

instance TypeCompare Type TypeOrErrors where
  _ <~ (Errors _) = return ()
  a <~ (Type b) = a <~ b -- This isn't right: this will flip the "expected/received"

instance TypeCompare TypeOrErrors TypeOrErrors where
  (Errors _) <~ _ = return ()
  (Type a) <~ b = a <~ b -- This isn't right: this will flip the "expected/received"

instance TypeCompare Type [Type] where
  typ <~ typs = mapM_ (typ<~) typs

foundType :: Monad m => Type -> m TypeOrErrors
foundType = return . Type

unify :: [Type] -> Type
unify [] = TNone
unify (x:_) = x -- TODO: Do this right


-- Knot tying implementation - it's awesome!
typeCheckAst :: AstMu -> (AstMc, Errors)
typeCheckAst ast = trace "typeCheckAst" $ runST $ do
  history <- newSTRef []
  rec result@(typedAst, _) <- runWriterT $ runReaderT
        (typeCheckUnitTable ast) $
        TypeContext (GlobalBindings typedAst) history
  return result

typeCheckUnitTable :: AstMu -> TypeCheckM s AstMc
typeCheckUnitTable unitTable = trace "typeCheckUnitTable" $
  multiMapWithKeyM typeCheckUnitLazy unitTable

typeCheckUnitLazy :: Name -> UnitMu -> TypeCheckM s UnitMc
typeCheckUnitLazy name unit = do
  env <- ask
  resultAndErrors <- lift $ lift $
    unsafeInterleaveST $
      runWriterT $
        runReaderT (typeCheckUnit name unit) env
  tell $ snd resultAndErrors
  return $ fst resultAndErrors

typeCheckUnit :: Name -> UnitMu -> TypeCheckM s UnitMc
typeCheckUnit name unit = trace "typeCheckUnit" $ do
  pushSearchName name
  res <- case unit of
    UNamespaceM n -> UNamespaceM <$> typeCheckUnitTable n
    UFuncM l -> UFuncM <$> typeCheckLambda l
    UVar v -> UVar <$> typeCheckVar v
  popSearchName
  return res


class EnforceOrInfer a b where
  enforceOrInfer :: a -> b -> TypeCheckM s TypeOrErrors

instance EnforceOrInfer (Maybe Type) TypeOrErrors where
  enforceOrInfer (Just prescribedType) assignedTypeRes = do
    prescribedType <~ assignedTypeRes
    foundType prescribedType
  enforceOrInfer Nothing assignedTypeRes = return assignedTypeRes

instance EnforceOrInfer (Maybe Type) Type where
  enforceOrInfer (Just prescribedType) assignedType = do
    prescribedType <~ assignedType
    foundType prescribedType
  enforceOrInfer Nothing assignedType = foundType assignedType


typeCheckLambda :: LambdaU -> TypeCheckM s LambdaC
typeCheckLambda (Lambda (SigU p params returnTypeMaybe) b) = do

  bindings <- getBindings
  let blockBindings = initBlockBindings bindings params

  (b', typesReturned) <- withBindings blockBindings $ typeCheckBlock b
  let typesReturnedUnified = unify $ mapMaybe getType typesReturned
  returnType <- enforceOrInfer returnTypeMaybe typesReturnedUnified
  return $ Lambda (SigC p params returnType) b'


-- Yields a type checked block and a list of returned types
typeCheckBlock :: BlockU -> TypeCheckM s (BlockC, [TypeOrErrors])
typeCheckBlock [] = return ([], [])
typeCheckBlock b@[SExpr expr] = do
  (exprChecked, exprTypeResult) <- typeCheckExpr expr
  return ([SExpr exprChecked], [exprTypeResult])

-- TODO: Maintain a context within the block.

typeCheckVar :: VarMu -> TypeCheckM s VarMc
typeCheckVar (VarMu lMut lTypeMaybe rhs) = do
  (rhsChecked, rhsTypeResult) <- typeCheckExpr rhs
  varType <- enforceOrInfer lTypeMaybe rhsTypeResult
  return $ VarMc lMut varType $ rhsChecked


typeCheckExpr :: ExprU -> TypeCheckM s (ExprC, TypeOrErrors)
typeCheckExpr expr = trace "typeCheckExpr" $ do
  case expr of
    EApp app -> do
      (appChecked, appType) <- typeCheckApp app
      return (EApp appChecked, appType)

    EName name -> do
      nameType <- typeCheckName name
      return (EName name, nameType)

    EIf e1 ec e2 -> do
      ((e1c, ecc, e2c), ifType) <- typeCheckIf e1 ec e2
      return (EIf e1c ecc e2c, ifType)

    EAdd e1 e2 -> do
      ((e1c, e2c), tp) <- typeCheckBinOp e1 e2
      return ((EAdd e1c e2c), tp)

    EMul e1 e2 -> do
      ((e1c, e2c), tp) <- typeCheckBinOp e1 e2
      return ((EMul e1c e2c), tp)

    ELitBln b -> return $ (ELitBln b, Type $ TBln)
    ELitFlt f -> return $ (ELitFlt f, Type $ TFlt)
    ELitInt i -> return $ (ELitInt i, Type $ TInt)
    ELitStr s -> return $ (ELitStr s, Type $ TStr)


typeCheckApp :: AppU -> TypeCheckM s (AppC, TypeOrErrors)
typeCheckApp (App e args) = trace "typeCheckApp" $ do
  (eChecked, eTypeRes) <- typeCheckExpr e
  (argsChecked, _) <- typeCheckArgs args -- todo: must account for the types
  let appChecked = App eChecked argsChecked
  case eTypeRes of
    Errors _ -> return undefined -- $ (App eChecked params, Errors [ErrorPropagated errors])
    Type eType -> case eType of
      TFunc _ _ ret -> return (appChecked, Type ret)
      _ -> do raise $ NonApplicable eType; return (appChecked, Errors [NonApplicable eType])

typeCheckApp' :: Purity -> [Type] -> Type -> TypeCheckM s TypeOrErrors
typeCheckApp' _ _ = return . Type -- Obvious madness and code smell here too

typeCheckArgs :: ArgsU -> TypeCheckM s (ArgsC, [TypeOrErrors])
typeCheckArgs (Args purity exprs) = do --(_, )
  exprResults <- mapM typeCheckExpr exprs
  return (Args purity $ map fst exprResults, map snd exprResults)

typeCheckName :: Name -> TypeCheckM s TypeOrErrors
typeCheckName name = trace ("typeCheckName " ++ name) $ do
  history <- getHistory
  traceM $ "searching for name " ++ name ++ " in " ++ show history
  if elem name history then do
    traceM $ "found "++name++" in "++show history++"; raising recursive definition"
    raise RecursiveDefinition
    return $ Errors [RecursiveDefinition]
  else do
    bindings <- getBindings
    findTypeName' $ lookupKinds bindings name
    where
      findTypeName' :: [Kind] -> TypeCheckM s TypeOrErrors
      findTypeName' ks = trace "findTypeName'" $ case ks of
        [] -> do
          raise $ UnknownId name
          return $ Errors [UnknownId name]
        [k] -> case k of
          KExpr t -> foundType t
          KType -> do
            raise NeedExprFoundType
            return $ Errors [NeedExprFoundType]
          KNamespace -> do
            raise NeedExprFoundNamespace
            return $ Errors [NeedExprFoundNamespace]
          KError es -> do
            return $ Errors [ErrorPropagated es]
        _ -> do
          raise $ CompetingDefinitions
          return $ Errors [CompetingDefinitions]

typeCheckIf :: ExprU -> ExprU -> ExprU -> TypeCheckM s ((ExprC, ExprC, ExprC), TypeOrErrors)
typeCheckIf e1 ec e2 = do
  (e1Checked, type1) <- typeCheckExpr e1
  (e2Checked, type2) <- typeCheckExpr e2
  (ecChecked, typeC) <- typeCheckExpr ec

  -- TODO: determine common root type, e.g. typeof(a if a.exists else none) == ?A
  type1 <~ type2   -- ensure that the types of the expressions are compatible

  case typeC of
    Type t -> do TBln <~ t
    Errors _ -> return ()
  return ((e1Checked, ecChecked, e2Checked), type1) -- Errors [])


-- TODO: I must generalize operators (hardcoding every possibility is not happening)

typeCheckBinOp :: ExprU -> ExprU -> TypeCheckM s ((ExprC, ExprC), TypeOrErrors)
typeCheckBinOp e1 e2 = do
  (e1Checked, type1) <- typeCheckExpr e1
  (e2Checked, type2) <- typeCheckExpr e2

  -- TODO: determine common root type, e.g. typeof(3 + 4.7) == TFlt
  type1 <~ type2 -- this is not right, but is better than nothing

  return ((e1Checked, e2Checked), type1)

