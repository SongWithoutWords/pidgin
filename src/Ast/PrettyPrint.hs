module Ast.PrettyPrint(formatAst) where

indentedNewline :: Int -> String
indentedNewline n = '\n' : replicate (2*n) ' '

formatAst :: String -> String
formatAst s = formatAst' s 0
  where
    formatAst' :: String -> Int -> String
    formatAst' [] _ = ""
    formatAst' (c:cs) n
      | c == '(' || c == '[' = formatAst' cs (n+1)
      | c == ')' || c == ']' = formatAst' cs (n-1)
      | c == ' ' = indentedNewline n ++ formatAst' cs n
      | c == ',' = indentedNewline (n-1) ++ formatAst' cs n
      | otherwise = c : formatAst' cs n

