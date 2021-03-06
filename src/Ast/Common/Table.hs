module Ast.Common.Table
  ( module Ast.Common.Name
  , module Ast.Common.Table
  ) where

import Ast.Common.Name
import Util.MultiMap

type Table a = MultiMap Name a

tableFromNamed :: [Named a] -> Table a
tableFromNamed = multiFromList

tableFromNamedM :: Monad m => (a -> m b) -> [Named a] -> m (Table b)
tableFromNamedM f xs =
  mapM (\(n, a) -> f a >>= \b -> pure $ (n, b)) xs >>= pure . multiFromList

