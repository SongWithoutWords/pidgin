{-# language MultiParamTypeClasses #-}
{-# language TypeOperators #-}

module Util.Preface where

import Control.Monad(liftM2)
import Data.Monoid((<>))

-- Might like to change this operator to ×
-- type a & b = (a, b)

-- infixr 0 &
-- (&) :: a -> b -> (a, b)
-- (&) x y = (x, y)

identity :: a -> a
identity x = x

-- Maybes
----------------------------------------------------------------
orElse :: Maybe a -> a -> a
orElse (Just x) _ = x
orElse Nothing y = y

infixr 2 ??
(??) :: Maybe a -> a -> a
(??) = orElse

consMaybe :: Maybe a -> [a] -> [a]
consMaybe (Just x) xs = x : xs
consMaybe Nothing xs = xs

(?:) :: Maybe a -> [a] -> [a]
(?:) = consMaybe

-- left-biased choice on maybes
(<?>) :: Maybe a -> Maybe a -> Maybe a
Just x <?> _ = Just x
_ <?> Just y = Just y
_ <?> _ = Nothing

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe _ [] = Nothing
findMaybe f (x:xs) = case f x of
  Just y -> Just y
  Nothing -> findMaybe f xs

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x



-- Collections
----------------------------------------------------------------
alleq :: Eq a => [a] -> Bool
alleq [] = True
alleq (x:xs)
  | xs == [] = True
  | x == head xs = alleq xs
  | otherwise = False

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x
  then takeWhileInclusive p xs
  else []

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast [x] = Just x
maybeLast (_:xs) = maybeLast xs


-- Functions
----------------------------------------------------------------

-- Composition with two inputs
(.:) :: (a -> b) -> (x -> y -> a) -> x -> y -> b
(.:) f g x y = f $ g x y


-- Monads
----------------------------------------------------------------
concatMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
concatMapM _ [] = pure mempty
concatMapM f (x:xs) = liftM2 (<>) (f x) (concatMapM f xs)
