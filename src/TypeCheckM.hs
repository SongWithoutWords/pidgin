module TypeCheckM
  ( module TypeCheckM
  , module Writer
  , module Reader
  , module TypeContext
  , module TypeErrors
  ) where

import Control.Monad.State
import Control.Monad.Writer as Writer
import Control.Monad.Trans.Reader as Reader

import TypeContext
import TypeErrors
import Types

type ReadWriteM r w a = ReaderT r (Writer w) a
type TypeCheckM a = ReadWriteM TypeContext Errors a

runTypeCheck :: TypeContext -> TypeCheckM a -> (a, Errors)
runTypeCheck context typeCheckM = runWriter $ runReaderT typeCheckM context

raise :: Error -> TypeCheckM ()
raise e = tell [e]

foundType :: Monad m => Type -> m TypeOrErrors --(Maybe a)
foundType = return . Type

type CheckedAst = ()
type UncheckedAst = ()

type StateReaderM r s a = ReaderT r (State s) a
type TypeCheckState a = StateReaderM UncheckedAst CheckedAst a


