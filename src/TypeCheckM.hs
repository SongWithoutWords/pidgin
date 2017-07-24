module TypeCheckM
  ( module TypeCheckM
  , module Writer
  , module Reader
  , module TypeContext
  , module TypeErrors
  ) where

import Control.Monad.Writer as Writer
import Control.Monad.Trans.Reader as Reader

import TypeContext
import TypeErrors

type ReadWriteM r w a = ReaderT r (Writer w) a
type TypeCheckM a = ReadWriteM TypeContext Errors a

runTypeCheck :: TypeContext -> TypeCheckM a -> (a, Errors)
runTypeCheck context typeCheckM = runWriter $ runReaderT typeCheckM context

raise :: Error -> TypeCheckM ()
raise e = tell [e]

found :: Monad m => a -> m (Maybe a)
found = return . Just

