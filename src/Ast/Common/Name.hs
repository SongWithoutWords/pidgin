{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
module Ast.Common.Name where

type Name = String
type Names = [Name]

type Named a = (Name, a)

-- data Named a = Named Name a
  -- deriving(Eq, Ord, Show)

-- instance Functor ((,) Name) where
--   fmap f (n, a) = (n, f a)

-- instance Foldable ((,) Name) where
--   foldMap f (_, a) = f a

-- instance Traversable ((,) Name) where
--   traverse f (n, a) = (,) n <$> f a

-- lookupName :: Name -> [Named a] -> Maybe a
-- lookupName _ [] = Nothing
-- lookupName name ((n, x) : rest) =
--   if n == name then Just x
--   else lookupName name rest


