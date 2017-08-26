{-# language TypeOperators #-}

module Preface where

-- Might like to change this operator to ×
type a & b = (a, b)

infixr 0 &
(&) :: a -> b -> (a, b)
(&) x y = (x, y)

identity :: a -> a
identity x = x

orElse :: Maybe a -> Maybe a -> Maybe a
orElse x@Just{} _ = x
orElse Nothing y = y

(??) :: Maybe a -> Maybe a -> Maybe a
(??) = orElse

consMaybe :: Maybe a -> [a] -> [a]
consMaybe (Just x) xs = x : xs
consMaybe Nothing xs = xs

(?:) :: Maybe a -> [a] -> [a]
(?:) = consMaybe

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x
  then takeWhileInclusive p xs
  else []

