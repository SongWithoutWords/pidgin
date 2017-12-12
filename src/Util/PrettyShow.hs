module Util.PrettyShow(prettyShow) where

prettyShow :: Show a => a -> String
prettyShow x = prettyShow' (show x) 0
  where

prettyShow' :: String -> Int -> String
prettyShow' [] _ = ""
prettyShow' (c:cs) n
  | c == '(' || c == '[' = indentedNewline n ++ c : prettyShow' cs (n+1)
  | c == ')' || c == ']' = c : prettyShow' cs (n-1)
  | otherwise = c : prettyShow' cs n

indentedNewline :: Int -> String
indentedNewline n = '\n' : replicate (2*n) ' '
