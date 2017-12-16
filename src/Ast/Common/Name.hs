module Ast.Common.Name where

type Name = String
type Names = [Name]

data Named a = Named Name a
  deriving(Eq, Ord, Show)

instance Functor Named where
  fmap f (Named n a) = Named n $ f a

instance Foldable Named where
  foldMap f (Named _ a) = f a

instance Traversable Named where
  traverse f (Named n a) = Named n <$> f a

lookupName :: Name -> [Named a] -> Maybe a
lookupName _ [] = Nothing
lookupName name (Named n x : rest) =
  if n == name then Just x
  else lookupName name rest


