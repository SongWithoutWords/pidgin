
module MultiMap
  ( module MultiMap
  , Map
  ) where

import Data.Map

type MultiMap k a = Map k [a]

multiLookup :: Ord k => k -> Map k [a] -> [a]
multiLookup k m = case Data.Map.lookup k m of
  Nothing -> []
  Just values -> values

multiInsert :: Ord k => k -> a -> Map k [a] -> Map k [a]
multiInsert k v = insertWith (++) k [v]

multiFromList :: Ord k => [(k, a)] -> Map k [a]
multiFromList pairs = Prelude.foldl (\m (k, v) -> multiInsert k v m) (Data.Map.empty) pairs

multiMapWithKeyM :: (Monad m, Ord k)  => (k -> a -> m b) -> Map k [a] -> m (Map k [b])
multiMapWithKeyM f multiMap = sequence $ mapWithKey (\k xs -> mapM (f k) xs) multiMap

