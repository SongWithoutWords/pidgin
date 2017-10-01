module Ast.Name where

type Name = String
type Names = [Name]

data Named a = Named Name a
  deriving(Eq, Show)

instance Functor Named where
  fmap f (Named n a) = Named n $ f a

instance Foldable Named where
  foldMap f (Named _ a) = f a

instance Traversable Named where
  traverse f (Named n a) = Named n <$> f a
