module TypeCheckM
  ( TypeCheckM
  , module Control.Monad
  , module TypeContext

  , TypeContext(..)

  -- Imported monads
  , evalStateT
  , runWriterT
  , runST
  , newSTRef

  -- Typecheck monad
  , typeCheckLazy
  , getBindings
  , modifyBindings
  , withBindings
  , getHistory
  , pushSearchName
  , popSearchName
  , raise
  , foundError

  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Monad.Writer
import Data.STRef

import Ast
import Debug
import TypeContext

-- TODO: make the switch to RWST with:
--    Reader History
--    State Bindings
--    Writer Errors

-- You could at that point consider a type class for bindings and get more
-- type safety (e.g. both local and global bindings can be queried,
-- but only local bindings can be modified on the fly)

type HistoryRef s = STRef s [Name]

data TypeContext s = TypeContext Bindings (HistoryRef s)

type TypeCheckM s a = StateT (TypeContext s) (WriterT Errors (ST s)) a


typeCheckLazy :: TypeCheckM s a -> TypeCheckM s a
typeCheckLazy typeCheckM = do
  env <- get
  resultAndErrors <- lift $ lift $
    unsafeInterleaveST $
      runWriterT $
        evalStateT typeCheckM env
  tell $ snd resultAndErrors
  return $ fst resultAndErrors


getBindings :: TypeCheckM s Bindings
getBindings = do
  TypeContext bindings _ <- get
  return bindings

modifyBindings :: (Bindings -> Bindings) -> TypeCheckM s ()
modifyBindings f = modify
  (\(TypeContext bindings history) -> TypeContext (f bindings) history)

withBindings :: Bindings -> TypeCheckM s a -> TypeCheckM s a
withBindings newBindings = withStateT
  (\(TypeContext _ history)->TypeContext newBindings history)


getHistoryRef :: TypeCheckM s (HistoryRef s)
getHistoryRef = do
  TypeContext _ historyRef <- get
  return historyRef

getHistory :: TypeCheckM s [Name]
getHistory = do
  ref <- getHistoryRef
  lift $ lift $ readSTRef ref

modifyHistory :: ([Name] -> [Name]) -> TypeCheckM s ()
modifyHistory f = do
  ref <- getHistoryRef
  lift $ lift $ modifySTRef ref f

pushSearchName :: Name -> TypeCheckM s ()
pushSearchName name = do
  modifyHistory $ \names -> name:names
  history <- getHistory
  traceM $ "push " ++ name ++ " -> " ++ show history

popSearchName :: TypeCheckM s ()
popSearchName = do
  modifyHistory $ \(_:names) -> names
  history <- getHistory
  traceM $ "pop:  " ++ show history

raise :: Error -> TypeCheckM s ()
raise e = tell [e]

foundError :: Error -> TypeCheckM s TypeT
foundError err = do
  raise err
  return $ TError err


