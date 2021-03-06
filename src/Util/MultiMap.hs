
module Util.MultiMap
  ( module Util.MultiMap
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

multiEmpty :: M.Map k [a]
multiEmpty = M.empty

multiFromList :: Ord k => [(k, a)] -> M.Map k [a]
multiFromList pairs = foldl (\m (k, v) -> multiInsert k v m) (M.empty) pairs

multiFromListCalcKey :: Ord k => (a -> k) -> [a] -> M.Map k [a]
multiFromListCalcKey f vals = foldl (\m v -> multiInsert (f v) v m) (M.empty) vals

multiToAscList :: Ord k => M.Map k [a] -> [(k, a)]
multiToAscList m = concatMap split $ M.toAscList m
  where
    split :: (k, [a]) -> [(k, a)]
    split (k, as) = map (\a -> (k, a)) as

multiValues :: Ord k => M.Map k [a] -> [a]
multiValues = concat . M.elems

multiMap :: Ord k => (a -> b) -> MultiMap k a -> MultiMap k b
multiMap f m = M.map (map f) m

multiMapM :: (Monad m, Ord k) => (a -> m b) -> M.Map k [a] -> m (M.Map k [b])
multiMapM f m = sequence $ M.map (mapM f) m

multiMapWithKey :: (k -> a -> b) -> M.Map k [a] -> M.Map k [b]
multiMapWithKey f = M.mapWithKey (\k xs -> map (f k) xs)

multiMapFoldWithKey :: (k -> a -> b) -> M.Map k [a] -> [b]
multiMapFoldWithKey f = M.foldMapWithKey (\k xs -> map (f k) xs)

multiMapWithKeyM :: (Monad m, Ord k)  => (k -> a -> m b) -> M.Map k [a] -> m (M.Map k [b])
multiMapWithKeyM f m = sequence $ M.mapWithKey (\k xs -> mapM (f k) xs) m

