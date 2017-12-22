module TypeCheck.ConstrainM
  ( ConstrainM
  , runConstrainM
  , ($=)
  , raise
  , pushNewScope
  , popScope
  , addLocalBinding
  , getNextTypeVar
  , lookupKinds
  ) where

import Control.Monad.RWS
import qualified Data.Map as M
import qualified Data.Set as S

import Ast.A2Constrained
import Ast.A2Constrained.Error
import TypeCheck.Constraint
import TypeCheck.Kind
import TypeCheck.Util
import Util.MultiMap


type Scope = M.Map Name Kind
type Scopes = [Scope]

data ConstrainState = ConstrainState
  { scopes :: Scopes
  , nextTypeId :: TVar
  , errors :: Errors
  }

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
addLocalBinding (Named n t) = modifyCurrentScope $ M.insert n $ KVar t

getNextTypeVar :: ConstrainM Type
getNextTypeVar = do
  val <- (gets nextTypeId)
  modify $ \s -> s{nextTypeId = (val + 1)}
  pure $ TVar val

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
      UClass _ -> KType
      UFunc f -> KVar $ typeOfFunc f
      UVar (Var t _) -> KVar t

    kindOfIntrinsic :: Intrinsic -> Kind
    kindOfIntrinsic = KVar . typeOfIntrinsic

  in do
    let intrins = multiLookup name intrinsicsByName
    globals <- multiLookup name <$> ask
    locals <- lookupLocal <$> gets scopes
    pure $ (kindOfIntrinsic <$> intrins) <> (kindOfUnit <$> globals) <> locals

