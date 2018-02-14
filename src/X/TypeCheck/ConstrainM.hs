{-# language LambdaCase #-}

module X.TypeCheck.ConstrainM
  ( ConstrainM
  , runConstrainM
  , runST
  , constrainStateToAst
  , pushSearchNode
  , popSearchNode
  , addFunc
  , pushNewScope
  , popScope
  , addLocalBinding
  , getNextTVar
  , getNextTypeVar
  , exprOfUnit
  , lookupName

  , checkType
  , applyMut
  ) where

-- import Control.Monad.RWS
import Control.Monad.Trans(lift)
import Control.Monad.ST
-- import Control.Monad.Trans.State
import Control.Monad.Trans.RWS
import Data.Monoid((<>))
import Data.STRef
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Ast.A0Parse.Type as A1
import X.Ast
import X.Error
import X.TypeCheck.Util
import Util.MultiMap
import Util.Preface


type Scope = M.Map Name Expr
type Scopes = [Scope]

-- data C

data ConstrainState = ConstrainState
  { ast :: Ast
  , fList :: [Func]
  , fCount :: FuncId
  , tList :: [Type]
  , tCount :: TypeId
  , vList :: [Var]
  , vCount :: VarId
  , scopes :: Scopes
  , nextTypeId :: TVar
  , errors :: Errors
  }
  deriving(Show)

initialState :: Ast -> ConstrainState
initialState a = ConstrainState
  { ast = a
  , fList = []
  , tList = []
  , vList = []
  , scopes = []
  , nextTypeId = 0
  , errors = S.empty
  }

-- type ConstrainM a = RWS Ast () ConstrainState a
type ConstrainM s = RWST (STRef s [Unit]) () ConstrainState (ST s)

-- type ConstrainM s = StateT (STRef s [Unit]) (ConstrainState s) (ST s)


runConstrainM :: ConstrainM s a -> Ast -> ST s (a, ConstrainState)
runConstrainM constrainM ast = do
  history <- newSTRef []
  (x, s, _) <- runRWST constrainM history (initialState ast)
  pure (x, s)
  -- let (x, s) = runST $ runStateT (initialState ast) constrainM --constrainM (initialState ast)
  -- in (x, s)

constrainStateToAst :: Namespace -> ConstrainState -> Ast
constrainStateToAst namespace state =
  let
    -- Reverse the lists so they match the order of the indices we've provided
    fs = fromList $ reverse $ fList state
    ts = fromList $ reverse $ tList state
    vs = fromList $ reverse $ vList state
  in Ast namespace fs ts vs

-- getNextFId :: ConstrainM s FuncId
-- getNextFId = do
--   fId@(FuncId curCount) <- gets fCount
--   modify $ \s -> s{fCount = FuncId $ curCount + 1}
--   pure fId

pushSearchNode :: Unit -> ConstrainM s ()
pushSearchNode u = do
  history <- ask
  lift $ modifySTRef history (u:)

popSearchNode :: ConstrainM s ()
popSearchNode = do
  history <- ask
  lift $ modifySTRef history tail

addFunc :: (FuncId -> ConstrainM s Func) -> ConstrainM s FuncId
addFunc f = do
  fId@(FuncId curCount) <- gets fCount
  f' <- f fId
  modify $ \s -> s{fList = f' : fList s, fCount = FuncId $ curCount + 1}
  pure $ fId

pushScope :: Scope -> ConstrainM s ()
pushScope scope = modify $ \s -> s{scopes = scope : scopes s}

pushNewScope :: ConstrainM s ()
pushNewScope = pushScope M.empty

popScope :: ConstrainM s ()
popScope = modify $ \s -> s{scopes = tail $ scopes s}

modifyCurrentScope :: (Scope -> Scope) -> ConstrainM s ()
modifyCurrentScope f = modify $ \s -> s{scopes = (f $ head $ scopes s):scopes s}

addLocalBinding :: Named Type -> ConstrainM s ()
addLocalBinding (n, t) = modifyCurrentScope $ M.insert n $ Expr t $ EBinding

getNextTVar :: ConstrainM s TVar
getNextTVar = do
  tvar <- (gets nextTypeId)
  modify $ \s -> s{nextTypeId = (tvar + 1)}
  pure tvar

getNextTypeVar :: ConstrainM s Type
getNextTypeVar = TVar <$> getNextTVar

intrinsicsByName :: MultiMap Name Intrinsic
intrinsicsByName = multiFromList $ zip (nameOfIntrinsic <$> intrinsics) intrinsics

exprOfUnit :: Unit -> ConstrainM s Expr
exprOfUnit u = case u of
  UNamespace units -> pure $ ENamespace units
  -- UType ms -> KType ms

  -- TODO: Account for recursive lookups
  UFunc fId -> do
    func <- (!fId) <$> functions <$> gets ast
    pure $ Expr (typeOfFunc func) (EName $ UFunc fId)

  UVar vId -> exprOfVar <$> (!vId) <$> vars <$> gets ast

lookupName :: Name -> ConstrainM s [Expr]
lookupName name =
  let
    lookupLocal :: Scopes -> [Expr]
    lookupLocal [] = []
    lookupLocal (l:ls) = case M.lookup name l of
      Just k -> [k]
      Nothing -> lookupLocal ls

    -- kindOfIntrinsic :: Intrinsic -> ConstrainM 
    -- kindOfIntrinsic i = do
    --   t <- checkType $ typeOfIntrinsic i
    --   pure $ KExpr $ Expr t $ EIntr i

  in do
    -- let intrins = UIntr <$> multiLookup name intrinsicsByName
    globals <- (multiLookup name <$> namespace <$> gets ast) >>= mapM exprOfUnit
    locals <- lookupLocal <$> gets scopes
    pure $ globals ++ locals

type TypeParamSubs = M.Map Typename TVar

checkType :: A1.Type -> ConstrainM s Type
checkType = checkType' M.empty
  where
  checkType' :: TypeParamSubs -> A1.Type -> ConstrainM s Type
  checkType' subs typ = let checkType'' = checkType' subs in case typ of

    -- Yep, I know it's wrong, will generalize at some stage ;)
    A1.TArgs [t] (A1.TUser "Array") -> TArray <$> checkType'' t

    -- A1.TUser name -> do
    --   let typeParamSub = M.lookup name subs
    --   results <- lookupUnits name
    --   let kinds = (case typeParamSub of Nothing -> []; Just tvar -> [KType $ TVar tvar])
    --             ++ results
    --   pure $ TName name kinds

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

