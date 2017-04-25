module Preface where

(&) :: a -> b -> (a, b)
(&) x y = (x, y)

identity :: a -> a
identity x = x

orElse :: Maybe a -> Maybe a -> Maybe a
orElse x@Just{} _ = x
orElse Nothing y = y

