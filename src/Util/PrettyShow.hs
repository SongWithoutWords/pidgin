module Util.PrettyShow(prettyShow) where

prettyShow :: Show a => a -> String
prettyShow x = prettyShow' (show x) 0 True
  where

prettyShow' :: String -> Int -> Bool -> String
prettyShow' [] _ _ = ""
prettyShow' (c:cs) n lastCharWasOpener

  | c == '(' || c == '[' =
    (if lastCharWasOpener then " " else indentedNewline n)
    ++ c : prettyShow' cs (n+1) True

  | c == ')' || c == ']' = c : prettyShow' cs (n-1) False

  | c == ',' = indentedNewline (n - 1) ++ c : prettyShow' cs n True

  | otherwise = c : prettyShow' cs n False

indentedNewline :: Int -> String
indentedNewline n = '\n' : replicate (2*n) ' '
