{-# language TypeOperators #-}

module Preface where

-- Might like to change this operator to ×
type a & b = (a, b)

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

