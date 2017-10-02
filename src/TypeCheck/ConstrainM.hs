{-# language GADTs #-}
module TypeCheck.ConstrainM
  ( module TypeCheck.ConstrainM
  ) where

import Control.Monad.RWS
import qualified Data.Map as M

import MultiMap

import Ast

import TypeCheck.Constraint

type Scope = M.Map Name Kind
type Scopes = [Scope]


data ConstrainState = ConstrainState
  { scopes :: Scopes
  , nextTypeId :: Word
  }

initialState :: ConstrainState
initialState = ConstrainState
  { scopes = []
  , nextTypeId = 0
  }

type ConstrainM a = RWS Ast2 [Constraint] ConstrainState a

runConstrainM :: ConstrainM a -> Ast2 -> (a, [Constraint])
runConstrainM constrainM ast =
  let (x, _, constraints) = runRWS constrainM ast initialState
  in (x, constraints)

constrain :: Type2 -> Type2 -> ConstrainM ()
constrain t1 t2 = tell [t1 := t2]


pushScope :: Scope -> ConstrainM ()
pushScope scope = modify $ \s -> s{scopes = scope : scopes s}

pushNewScope :: ConstrainM ()
pushNewScope = pushScope M.empty

popScope :: ConstrainM ()
popScope = modify $ \s -> s{scopes = tail $ scopes s}

modifyCurrentScope :: (Scope -> Scope) -> ConstrainM ()
modifyCurrentScope f = modify $ \s -> s{scopes = (f $ head $ scopes s):scopes s}

pushLocal :: Name -> Type2 -> ConstrainM ()
pushLocal n t = modifyCurrentScope $ M.insert n (KExpr t)

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

      UFunc (Func1 (Sig2 purity params returnType) _) ->
        KExpr (TFunc purity (map (\(Param _ t _) -> t) params) returnType)

      UVar (Var2 _ typ _) -> KExpr typ

  pure $ lookupKinds' locals

