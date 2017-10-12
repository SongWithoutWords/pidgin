module Util.UnorderedPair where

data UnorderedPair a = UnorderedPair a a
  deriving(Show)

instance Eq a => Eq (UnorderedPair a) where
  (UnorderedPair a1 a2) == (UnorderedPair b1 b2) =
    a1 == b1 && a2 == b2 || a1 == b2 && a2 == b1

