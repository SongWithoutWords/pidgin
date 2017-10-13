{-# language GADTs #-}
module TypeCheck.ConstrainM
  ( ConstrainM
  , runConstrainM
  , constrain
  , raise
  , foundError
  , pushNewScope
  , popScope
  , addLocalBinding
  , getNextTypeVar
  , lookupKinds
  ) where

import Control.Monad.RWS
-- import Control.Monad.Writer
-- import Control.Monad.State
-- import Control.Monad.Reader
import qualified Data.Map as M

import Ast
import MultiMap
import TypeCheck.Constraint
import TypeCheck.Util


type Scope = M.Map Name Kind
type Scopes = [Scope]

data ConstrainState = ConstrainState
  { scopes :: Scopes
  , nextTypeId :: Word
  , errors :: Errors
  }

initialState :: ConstrainState
initialState = ConstrainState
  { scopes = []
  , nextTypeId = 0
  , errors = []
  }

type ConstrainM a = RWS Ast2 [Constraint] ConstrainState a

runConstrainM :: ConstrainM a -> Ast2 -> (a, [Constraint], Errors)
runConstrainM constrainM ast =
  let (x, s, constraints) = runRWS constrainM ast initialState
  in (x, constraints, errors s)

constrain :: Type2 -> Type2 -> ConstrainM ()
constrain t1 t2 = tell [t1 := t2]

raise :: Error -> ConstrainM ()
raise e = modify $ \s -> s{errors = e : errors s}

foundError :: Error -> ConstrainM Type2
foundError e = raise e >> pure TError

pushScope :: Scope -> ConstrainM ()
pushScope scope = modify $ \s -> s{scopes = scope : scopes s}

pushNewScope :: ConstrainM ()
pushNewScope = pushScope M.empty

popScope :: ConstrainM ()
popScope = modify $ \s -> s{scopes = tail $ scopes s}

modifyCurrentScope :: (Scope -> Scope) -> ConstrainM ()
modifyCurrentScope f = modify $ \s -> s{scopes = (f $ head $ scopes s):scopes s}

addLocalBinding :: Name -> Type2 -> ConstrainM ()
addLocalBinding n t = modifyCurrentScope $ M.insert n (KExpr t)

getNextTypeVar :: ConstrainM Type2
getNextTypeVar = do
  val <- (gets nextTypeId)
  modify $ \s -> s{nextTypeId = (val + 1)}
  pure $ TVar val

lookupKinds :: Name -> ConstrainM [Kind]
lookupKinds name = do
  locals <- gets scopes
  global <- ask
  let
    lookupKinds' :: Scopes -> [Kind]
    lookupKinds' [] = map kindOfUnit $ multiLookup name global
    lookupKinds' (l:ls) = case M.lookup name l of
      Just k -> [k]
      Nothing -> lookupKinds' ls

    kindOfUnit :: Unit2 -> Kind
    kindOfUnit u = case u of
      UNamespace1 _ -> KNamespace
      UClass _ -> KType
      UFunc f -> KExpr $ typeOfFunc f
      UVar (Var2 _ typ _) -> KExpr typ

  pure $ lookupKinds' locals

