module ParseUtil where

import Ast

tupleToNamed :: (a -> b) -> (Name, a) -> Named b
tupleToNamed f (n, x) = Named n $ f x
