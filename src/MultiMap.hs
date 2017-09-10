
module MultiMap
  ( module MultiMap
  , M.Map
  ) where

import qualified Data.Map as M

type MultiMap k a = M.Map k [a]

multiLookup :: Ord k => k -> M.Map k [a] -> [a]
multiLookup k m = case M.lookup k m of
  Nothing -> []
  Just values -> values

multiInsert :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a]
multiInsert k v = M.insertWith (++) k [v]

multiFromList :: Ord k => [(k, a)] -> M.Map k [a]
multiFromList pairs = Prelude.foldl (\m (k, v) -> multiInsert k v m) (M.empty) pairs

multiMapWithKey :: (k -> a -> b) -> M.Map k [a] -> M.Map k [b]
multiMapWithKey f multi = M.mapWithKey (\k xs -> Prelude.map (f k) xs) multi

multiMapWithKeyM :: (Monad m, Ord k)  => (k -> a -> m b) -> M.Map k [a] -> m (M.Map k [b])
multiMapWithKeyM f multiMap = sequence $ M.mapWithKey (\k xs -> mapM (f k) xs) multiMap

