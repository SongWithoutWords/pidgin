
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
multiFromList pairs = Prelude.foldl (\m (k, v) -> multiInsert k v m) (M.empty) pairs

multiMap :: Ord k => (a -> b) -> MultiMap k a -> MultiMap k b
multiMap f m = M.map (map f) m

multiMapM :: (Monad m, Ord k) => (a -> m b) -> M.Map k [a] -> m (M.Map k [b])
multiMapM f m = sequence $ M.map (mapM f) m

multiMapWithKey :: (k -> a -> b) -> M.Map k [a] -> M.Map k [b]
multiMapWithKey f = M.mapWithKey (\k xs -> Prelude.map (f k) xs)

multiMapFoldWithKey :: (k -> a -> b) -> M.Map k [a] -> [b]
multiMapFoldWithKey f = M.foldMapWithKey (\k xs -> Prelude.map (f k) xs)

multiMapWithKeyM :: (Monad m, Ord k)  => (k -> a -> m b) -> M.Map k [a] -> m (M.Map k [b])
multiMapWithKeyM f m = sequence $ M.mapWithKey (\k xs -> mapM (f k) xs) m

