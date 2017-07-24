{-# language TypeSynonymInstances #-}
{-# language InstanceSigs #-}

module MultiMapInstances
  ( module MultiMapInstances
  , module Data.MultiMap
  ) where

import Data.MultiMap

instance (Eq k, Eq a) => Eq (MultiMap k a) where
  (==) x y = toMap x == toMap y

instance (Show k, Show a) => Show (MultiMap k a) where
  show = show . toMap

union :: MultiMap k a -> MultiMap k a -> MultiMap k a
union = undefined

instance Monoid (MultiMap k a) where
  mempty = empty
  mappend = union

instance Functor (MultiMap k) where
  fmap :: (a -> b) -> MultiMap k a -> MultiMap k b
  fmap = undefined

instance Foldable (MultiMap k) where
  foldr :: (a -> b -> b) -> b -> (MultiMap k a) -> b
  foldr = undefined

instance Traversable (MultiMap k) where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse = undefined

