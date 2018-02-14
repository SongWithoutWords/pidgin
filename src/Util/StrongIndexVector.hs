module Util.StrongIndexVector where

import qualified Data.Vector as V

class Index i where
  toInt :: i -> Int

newtype StrongIndexVector i a = StrongIndexVector (V.Vector a)
  deriving(Eq, Ord, Show)

(!?) :: Index i => StrongIndexVector i a -> i -> Maybe a
(!?) (StrongIndexVector v) i = v V.!? (toInt i)

(!) :: Index i => StrongIndexVector i a -> i -> a
(!) (StrongIndexVector v) i = v V.! (toInt i)

fromList :: (Index i) => [a] -> StrongIndexVector i a
fromList = StrongIndexVector . V.fromList

