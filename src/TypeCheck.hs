{-# language GADTs #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RecursiveDo #-}

module TypeCheck(typeCheckAst) where

import Data.Maybe

import Preface

import Ast
import Debug
import MultiMap
import TypeCheckM


-- Is type b assignable to type a?
class TypeCompare a b where
  (<~) :: a -> b -> TypeCheckM s ()

-- I think I can probably generalize the rhs to a functor. I think.
instance TypeCompare Type Type where
  a <~ b = when (a /= b) $ raise TypeConflict { typeRequired = a, typeFound = b }

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
  rec result@(typedAst, _) <- runWriterT $ evalStateT
        (typeCheckUnitTable ast) $
        TypeContext (GlobalBindings typedAst) history
  return result

typeCheckUnitTable :: AstMu -> TypeCheckM s AstMc
typeCheckUnitTable unitTable = trace "typeCheckUnitTable" $
  multiMapWithKeyM (\name unit -> typeCheckLazy $ typeCheckUnit name unit) unitTable

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
typeCheckLambda (Lambda (SigU p params returnTypeMaybe) returnStyle block) = do
  bindings <- getBindings
  let blockBindings = initBlockBindings bindings params

  (block', typesReturned) <- withBindings blockBindings $ typeCheckBlock block returnStyle

  let typesReturnedUnified = unify $ mapMaybe getType typesReturned
  returnType <- enforceOrInfer returnTypeMaybe typesReturnedUnified
  return $ Lambda (SigC p params returnType) returnStyle block'

-- Yields a type checked block and a list of returned types
typeCheckBlock :: [StmtU] -> RetNotation -> TypeCheckM s ([StmtC], [TypeOrErrors])
typeCheckBlock [] _ = return ([], [])
typeCheckBlock [SExpr e] ImplicitRet = do
  (e', eType) <- typeCheckExpr e
  return ([SExpr e'], [eType])
typeCheckBlock (stmt:stmts) retStyle = do
  (stmt', maybeRet) <- typeCheckStmt stmt
  (stmts', rets) <- typeCheckBlock stmts retStyle
  return (stmt':stmts', maybeRet ?: rets)

typeCheckStmt :: StmtU -> TypeCheckM s (StmtC, Maybe TypeOrErrors)
typeCheckStmt stmt = case stmt of

  -- TODO: will need to account for mutations in future
  SAssign lexpr expr -> undefined

  SVar (VarLu mut optionalType name e) -> do
    (e', eType) <- typeCheckExpr e
    varType <- enforceOrInfer optionalType eType
    modifyBindings $ addLocalBinding name $ case eType of
      Type t -> KExpr t
      Errors es -> KError es
    return (SVar $ VarLc mut varType name e', Nothing)

  SFunc f -> undefined

  SIf ifBranch -> undefined

  -- TODO: detect useless expressions
  SExpr e -> do
    (e', _) <- typeCheckExpr e
    return (SExpr e', Nothing)

  SRet e -> undefined



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

    ESub e1 e2 -> do
      ((e1c, e2c), tp) <- typeCheckBinOp e1 e2
      return ((EAdd e1c e2c), tp)

    EMul e1 e2 -> do
      ((e1c, e2c), tp) <- typeCheckBinOp e1 e2
      return ((EMul e1c e2c), tp)

    ELesserEq e1 e2 -> do
      ((e1c, e2c), tp) <- typeCheckComparison e1 e2
      return ((ELesserEq e1c e2c), tp)

    ELitBln b -> return $ (ELitBln b, Type $ TBln)
    ELitFlt f -> return $ (ELitFlt f, Type $ TFlt)
    ELitInt i -> return $ (ELitInt i, Type $ TInt)
    ELitStr s -> return $ (ELitStr s, Type $ TStr)


typeCheckApp :: AppU -> TypeCheckM s (AppC, TypeOrErrors)
typeCheckApp (App e args) = trace "typeCheckApp" $ do
  (eChecked, eTypeRes) <- typeCheckExpr e
  (argsChecked, argTypes) <- typeCheckArgs args -- todo: must account for the types
  let appChecked = App eChecked argsChecked
  case eTypeRes of
    Errors errors -> return $ (App eChecked argsChecked, Errors [ErrorPropagated errors])
    Type eType -> case eType of
      TFunc purity paramTypes ret -> do
        let (Args argPurity _) = argsChecked
        typeCheckApp' (purity, paramTypes) (argPurity, argTypes)
        return (appChecked, Type ret)

      _ -> do raise $ NonApplicable eType; return (appChecked, Errors [NonApplicable eType])

  where
    typeCheckApp' :: (Purity, [Type]) -> (Purity, [TypeOrErrors]) -> TypeCheckM s ()
    typeCheckApp' (paramPurity, paramTypes) (argPurity, argTypes) = do
      when (paramPurity /= argPurity) $ raise $ WrongPurity paramPurity argPurity

      if length paramTypes /= length argTypes then
        raise $ WrongNumArgs (length paramTypes) (length argTypes)
      else
        -- Pairwise comparison of each type
        zipWithM_ (<~) paramTypes argTypes


typeCheckArgs :: ArgsU -> TypeCheckM s (ArgsC, [TypeOrErrors])
typeCheckArgs (Args purity exprs) = do --(_, )
  exprResults <- mapM typeCheckExpr exprs
  return (Args purity $ map fst exprResults, map snd exprResults)

typeCheckName :: Name -> TypeCheckM s TypeOrErrors
typeCheckName name = trace ("typeCheckName " ++ name) $ do
  history <- getHistory
  traceM $ "searching for name " ++ name ++ " in " ++ show history
  if elem name history then do
    traceM $ "found " ++ name ++ " in " ++ show history ++
      "; raising recursive definition"
    let cycle = takeWhileInclusive (\x -> x /= name) history
    raise $ recursiveDefinition cycle
    return $ Errors [recursiveDefinition cycle]
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
          KError es -> case es of
            [RecursiveDefinition cycle] -> do
              history <- getHistory
              let sourceName = head history
              if elem sourceName cycle
              then return $ Errors [RecursiveDefinition cycle]
              else return $ Errors [ErrorPropagated es]
            _ -> trace "unrelated error" $ return $ Errors [ErrorPropagated es]
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
  (e1c, type1) <- typeCheckExpr e1
  (e2c, type2) <- typeCheckExpr e2

  -- TODO: support operations between unrelated types, e.g. scalar and vector
  type1 <~ type2 -- this is not right, but is better than nothing

  return ((e1c, e2c), type1)

typeCheckComparison :: ExprU -> ExprU -> TypeCheckM s ((ExprC, ExprC), TypeOrErrors)
typeCheckComparison e1 e2 = do
  (e1c, type1) <- typeCheckExpr e1
  (e2c, type2) <- typeCheckExpr e2
  type1 <~ type2
  return ((e1c, e2c), Type TBln)

