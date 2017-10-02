module TypeCheck.ConstrainM where

import Control.Monad.RWS

import Ast

import TypeCheck.Constraint


data ConstrainState = ConstrainState
  { localBindings :: [(Name, Type2)]
  , nextTypeId :: Word
  }

initialState :: ConstrainState
initialState = ConstrainState
  { localBindings = []
  , nextTypeId = 0
  }

type ConstrainM a = RWS Ast2 [Constraint] ConstrainState a

pushLocal :: Name -> Type2 -> ConstrainM ()
pushLocal n t = modify $ \s -> s{localBindings = (n, t) : localBindings s}

popLocal :: ConstrainM ()
popLocal = modify $ \s -> s{localBindings = tail $ localBindings s}

getNextTypeVar :: ConstrainM Type2
getNextTypeVar = do
  val <- (gets nextTypeId)
  modify $ \s -> s{nextTypeId = (val + 1)}
  pure $ TVar val

constrain :: Type2 -> Type2 -> ConstrainM ()
constrain t1 t2 = tell [t1 := t2]

