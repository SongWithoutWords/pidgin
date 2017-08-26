module TypeCheckM
  ( TypeCheckM
  , module TypeContext

  , TypeContext(..)

  -- Built-in monads
  , lift

  , runReaderT
  , runWriterT

  , runST
  , newSTRef

  -- Typecheck monad
  , typeCheckLazy

  , getBindings
  , withBindings

  , getHistory
  , pushSearchName
  , popSearchName

  , raise
  ) where

import Control.Monad.Reader
import Data.STRef
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Monad.Writer

import Ast
import Debug
import TypeContext

type HistoryRef s = STRef s [Name]

data TypeContext s = TypeContext Bindings (HistoryRef s)

type TypeCheckM s a = ReaderT (TypeContext s) (WriterT Errors (ST s)) a

typeCheckLazy :: TypeCheckM s a -> TypeCheckM s a
typeCheckLazy typeCheckM = do
  env <- ask
  resultAndErrors <- lift $ lift $
    unsafeInterleaveST $
      runWriterT $
        runReaderT typeCheckM env
  tell $ snd resultAndErrors
  return $ fst resultAndErrors

getBindings :: TypeCheckM s Bindings
getBindings = do
  TypeContext bindings _ <- ask
  return bindings

withBindings :: Bindings -> TypeCheckM s a -> TypeCheckM s a
withBindings newBindings = local
  (\(TypeContext _ history)->TypeContext newBindings history)

modifyHistory :: ([Name] -> [Name]) -> TypeCheckM s ()
modifyHistory f = do
  ref <- getHistoryRef
  lift $ lift $ modifySTRef ref f

getHistoryRef :: TypeCheckM s (HistoryRef s)
getHistoryRef = do
  TypeContext _ historyRef <- ask
  return historyRef

getHistory :: TypeCheckM s [Name]
getHistory = do
  ref <- getHistoryRef
  lift $ lift $ readSTRef ref

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
raise e = tell [e]-- return () -- TODO


