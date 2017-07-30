{-# language DataKinds #-}
{-# language GADTs #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module TypeCheck(typeCheckAst) where

import Data.Maybe

import Preface

import Ast hiding(Checked)
import Ast0ToAst1
import TypeCheckM


-- Is type b assignable to type a?
class TypeCompare a b where
  (<~) :: a -> b -> TypeCheckM ()

-- I think I can probably generalize the rhs to a functor. I think.
instance TypeCompare Type Type where
  a <~ b =  when (a /= b) $ raise TypeConflict { expected = a, received = b }

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

unify :: [Type] -> Type
unify [] = TNone
unify (x:_) = x -- TODO: Do this right

-- Knot tying implementation - it's awesome!
typeCheckAst :: AstLu -> (AstMc, Errors)
typeCheckAst ast = let
  untypedAst = mapAst ast
  result@(typedAst, _) = runTypeCheck (TypeContext [] $ GlobalBindings typedAst) $ typeCheckUnitTable untypedAst
  in result

typeCheckUnitTable :: Table UnitMu -> TypeCheckM (Table UnitMc)
typeCheckUnitTable unitTable = do
  unitsChecked <- mapM typeCheckUnits unitTable
  return unitsChecked

typeCheckUnits :: [UnitMu] -> TypeCheckM [UnitMc]
typeCheckUnits = mapM typeCheckUnit

typeCheckUnit :: UnitMu -> TypeCheckM UnitMc
typeCheckUnit unit = case unit of
    UNamespaceM n -> UNamespaceM <$> typeCheckUnitTable n
    UFuncM l -> UFuncM <$> typeCheckLambda l
    UVar v -> UVar <$> typeCheckVar v


class EnforceOrInfer a b where
  enforceOrInfer :: a -> b -> TypeCheckM TypeOrErrors

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


typeCheckLambda :: LambdaU -> TypeCheckM LambdaC
typeCheckLambda (Lambda (SigU p params returnTypeMaybe) b) = do
    (TypeContext history bindings) <- ask
    let blockContext = TypeContext history $ initBlockBindings bindings params
    (b', typesReturned) <- local (\ _ -> blockContext) $ typeCheckBlock b
    let typesReturnedUnified = unify $ mapMaybe getType typesReturned
    returnType <- enforceOrInfer returnTypeMaybe typesReturnedUnified
    return $ Lambda (SigC p params returnType) b'


-- Yields a type checked block and a list of returned types
typeCheckBlock :: BlockU -> TypeCheckM (BlockC, [TypeOrErrors])
typeCheckBlock [] = return ([], [])
typeCheckBlock b@[SExpr expr] = do
  (exprChecked, exprTypeResult) <- typeCheckExpr expr
  return ([SExpr exprChecked], [exprTypeResult])

-- TODO: Maintain a context within the block.

typeCheckVar :: VarMu -> TypeCheckM VarMc
typeCheckVar (VarMu lMut lTypeMaybe rhs) = do
  (rhsChecked, rhsTypeResult) <- typeCheckExpr rhs
  varType <- enforceOrInfer lTypeMaybe rhsTypeResult
  return $ VarMc lMut varType $ rhsChecked


typeCheckExpr :: ExprU -> TypeCheckM (ExprC, TypeOrErrors)
typeCheckExpr expr = case expr of

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


typeCheckApp :: AppU -> TypeCheckM (AppC, TypeOrErrors)
typeCheckApp (App e args) = do
  (eChecked, eTypeRes) <- typeCheckExpr e
  (argsChecked, _) <- typeCheckArgs args -- todo: must account for the types
  let appChecked = App eChecked argsChecked
  case eTypeRes of
    Errors _ -> return undefined -- $ (App eChecked params, Errors [ErrorPropagated errors])
    -- Nothing -> return Nothing
    Type eType -> case eType of
      TFunc _ _ ret -> return (appChecked, Type ret)
      _ -> do raise $ NonApplicable eType; return (appChecked, Errors [NonApplicable eType])

      -- _ -> do raise $ NonApplicable eType; return $ Errors [NonApplicable eType]

typeCheckApp' :: Purity -> [Type] -> Type -> TypeCheckM TypeOrErrors
typeCheckApp' _ _ = return . Type -- Obvious madness and code smell here too

typeCheckArgs :: ArgsU -> TypeCheckM (ArgsC, [TypeOrErrors])
typeCheckArgs (Args purity exprs) = do --(_, )
  exprResults <- mapM typeCheckExpr exprs
  return (Args purity $ map fst exprResults, map snd exprResults)

typeCheckName :: Name -> TypeCheckM TypeOrErrors
typeCheckName n = do

-- In order to carry these names with me, can I put them in the Monad, or am I out of luck?
-- Will this work, can this work?

  TypeContext searchHistory bindings <- ask

  if elem n searchHistory then do
     raise RecursiveDefinition; return $ Errors [RecursiveDefinition]
  else do
    local (pushSearchName n) $ findTypeName' $ lookupKinds bindings n

    where
      findTypeName' :: [Kind] -> TypeCheckM TypeOrErrors
      findTypeName' ks = case ks of
        [] -> do
          raise $ UnknownId n
          return $ Errors [UnknownId n]
        [k] -> case k of
          KExpr t -> foundType t
          KType -> do
            raise NeedExprFoundType
            return $ Errors [NeedExprFoundType]
          KNamespace -> do
            raise NeedExprFoundNamespace
            return $ Errors [NeedExprFoundNamespace]
        _ -> do
          raise $ CompetingDefinitions
          return $ Errors [CompetingDefinitions]

typeCheckIf :: ExprU -> ExprU -> ExprU -> TypeCheckM ((ExprC, ExprC, ExprC), TypeOrErrors)
typeCheckIf e1 ec e2 = do
  (e1Checked, type1) <- typeCheckExpr e1
  (e2Checked, type2) <- typeCheckExpr e2
  (ecChecked, typeC) <- typeCheckExpr ec

  -- TODO: determine common root type, e.g. typeof(a if a.exists else none) == ?A
  type1 <~ type2   -- ensure that the types of the expressions are compatible

  case typeC of
    Type t -> do TBln <~ t
    Errors _ -> return ()
  return ((e1Checked, ecChecked, e2Checked), Errors [])


-- TODO: I must generalize operators (hardcoding every possibility is not happening)

typeCheckBinOp :: ExprU -> ExprU -> TypeCheckM ((ExprC, ExprC), TypeOrErrors)
typeCheckBinOp e1 e2 = do
  (e1Checked, type1) <- typeCheckExpr e1
  (e2Checked, type2) <- typeCheckExpr e2

  -- TODO: determine common root type, e.g. typeof(3 + 4.7) == TFlt
  type1 <~ type2 -- this is not right, but is better than nothing

  return ((e1Checked, e2Checked), type1)

