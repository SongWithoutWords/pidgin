module Util.Cycle where

shiftLeft :: [a] -> [a]
shiftLeft [] = []
shiftLeft [x] = [x]
shiftLeft (x:xs) = xs ++ [x]

data Cycle a = Cycle [a]
  deriving(Ord, Show)

instance Eq a => Eq (Cycle a) where
  (Cycle xs) == (Cycle ys) = cycleEq xs
    where
      cycleEq xsShifted =
        if xsShifted == ys || xsShifted == reverse ys then True
        else if (shiftLeft xsShifted) == xs then False
        else cycleEq (shiftLeft xsShifted)

