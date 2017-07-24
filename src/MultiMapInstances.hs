{-# language TypeSynonymInstances #-}
{-# language InstanceSigs #-}

module MultiMapInstances where

import qualified Data.MultiMap as Multi

type MultiMap = Multi.MultiMap

instance (Eq k, Eq a) => Eq (MultiMap k a) where
  (==) x y = Multi.toMap x == Multi.toMap y

instance (Show k, Show a) => Show (MultiMap k a) where
  show = show . Multi.toMap

union :: MultiMap k a -> MultiMap k a -> MultiMap k a
union = undefined

instance Monoid (MultiMap k a) where
  mempty = Multi.empty
  mappend = union

instance Functor (MultiMap k) where
  fmap :: (a -> b) -> MultiMap k a -> MultiMap k b
  fmap = undefined

instance Foldable (Multi.MultiMap k) where
  foldr :: (a -> b -> b) -> b -> (MultiMap k a) -> b
  foldr = undefined

instance Traversable (Multi.MultiMap k) where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse = undefined

