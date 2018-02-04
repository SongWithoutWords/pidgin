{-# language LambdaCase #-}

module TypeCheck.ConstrainM
  ( ConstrainM
  , runConstrainM
  , ($=)
  , raise
  , pushNewScope
  , popScope
  , addLocalBinding
  , getNextTVar
  , getNextTypeVar
  , lookupKinds

  , checkType
  , applyMut
  ) where

import Control.Monad.RWS
import qualified Data.Map as M
import Data.Maybe(mapMaybe)
import qualified Data.Set as S

import qualified Ast.A0Parse.Type as A1
import Ast.A2Constrained
import Ast.A2Constrained.Error
import TypeCheck.Constraint
import TypeCheck.Kind
import TypeCheck.Util
import Util.MultiMap
import Util.Preface


type Scope = M.Map Name Kind
type Scopes = [Scope]

data ConstrainState = ConstrainState
  { scopes :: Scopes
  , nextTypeId :: TVar
  , errors :: Errors
  }
  deriving(Show)

initialState :: ConstrainState
initialState = ConstrainState
  { scopes = []
  , nextTypeId = 0
  , errors = S.empty
  }

type ConstrainM a = RWS Ast [Constraint] ConstrainState a

runConstrainM :: ConstrainM a -> Ast -> (a, [Constraint], Errors)
runConstrainM constrainM ast =
  let (x, s, constraints) = runRWS constrainM ast initialState
  in (x, constraints, errors s)

($=) :: Type -> Type -> ConstrainM ()
x $= y = tell [x :$= y]

raise :: Error -> ConstrainM ()
raise e = modify $ \s -> s{errors = S.insert e $ errors s}

pushScope :: Scope -> ConstrainM ()
pushScope scope = modify $ \s -> s{scopes = scope : scopes s}

pushNewScope :: ConstrainM ()
pushNewScope = pushScope M.empty

popScope :: ConstrainM ()
popScope = modify $ \s -> s{scopes = tail $ scopes s}

modifyCurrentScope :: (Scope -> Scope) -> ConstrainM ()
modifyCurrentScope f = modify $ \s -> s{scopes = (f $ head $ scopes s):scopes s}

addLocalBinding :: Named Type -> ConstrainM ()
addLocalBinding (n, t) = modifyCurrentScope $ M.insert n $ KExpr $ Expr t $ EName n

getNextTVar :: ConstrainM TVar
getNextTVar = do
  tvar <- (gets nextTypeId)
  modify $ \s -> s{nextTypeId = (tvar + 1)}
  pure tvar

getNextTypeVar :: ConstrainM Type
getNextTypeVar = TVar <$> getNextTVar

intrinsicsByName :: MultiMap Name Intrinsic
intrinsicsByName = multiFromList $ zip (nameOfIntrinsic <$> intrinsics) intrinsics

lookupKinds :: Name -> ConstrainM [Kind]
lookupKinds name =
  let
    lookupLocal :: Scopes -> [Kind]
    lookupLocal [] = []
    lookupLocal (l:ls) = case M.lookup name l of
      Just k -> [k]
      Nothing -> lookupLocal ls

    kindOfUnit :: Unit -> Kind
    kindOfUnit u = case u of
      UNamespace _ -> KNamespace
      UData _ -> KType $ TUser name
      UFunc f -> KExpr $ Expr (typeOfFunc f) $ EName name
      UVar (Var t _) -> KExpr $ Expr t $ EName name

    kindOfIntrinsic :: Intrinsic -> ConstrainM Kind
    kindOfIntrinsic i = do
      t <- checkType $ typeOfIntrinsic i
      pure $ KExpr $ Expr t $ EIntr i

  in do
    intrins <- mapM kindOfIntrinsic $ multiLookup name intrinsicsByName
    globals <- multiLookup name <$> ask
    locals <- lookupLocal <$> gets scopes
    pure $ intrins <> (kindOfUnit <$> globals) <> locals

type TypeParamSubs = M.Map Typename TVar

checkType :: A1.Type -> ConstrainM Type
checkType = checkType' M.empty
  where
  checkType' :: TypeParamSubs -> A1.Type -> ConstrainM Type
  checkType' subs typ = let checkType'' = checkType' subs in case typ of

    -- Yep, I know it's wrong, will generalize at some stage ;)
    A1.TArgs [t] (A1.TUser "Array") -> TArray <$> checkType'' t

    A1.TUser typename -> do
      let typeParamSub = M.lookup typename subs
      kinds <- lookupKinds typename
      let types = (case typeParamSub of Nothing -> []; Just tvar -> [TVar tvar])
                ++ mapMaybe (\case KType t -> Just t; _ -> Nothing) kinds
      case types of
        [] -> raise (UnknownTypeName typename) >> pure TError
        [t] -> return t
        _ -> raise (AmbiguousTypeName typename) >> pure TError

    A1.TFunc purity params ret -> do
      params' <- mapM checkType'' params
      ret' <- checkType'' ret
      return $ TFunc purity params' ret'

    A1.TRef m t -> TRef . (applyMut m) <$> checkType'' t
    A1.TArray t -> TArray <$> checkType'' t

    -- Convert named template parameters into type variables
    A1.TParams typenames t -> do
      newSubs <- M.fromList <$> mapM (\n -> do tvar <- getNextTVar; pure (n, tvar)) typenames
      checkType' (subs <> newSubs) t


    A1.TBln -> return TBln
    A1.TChr -> return TChr
    A1.TFlt -> return TFlt
    A1.TInt -> return TInt
    A1.TNat -> return TNat
    A1.TStr -> return TStr

    A1.TNone -> return TNone

applyMut :: Mut -> Type -> Type
applyMut Mut = TMut
applyMut _ = identity

