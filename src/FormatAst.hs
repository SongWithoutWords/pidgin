module FormatAst (formatAst, formatTokens) where

import Tokens

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

formatTokens :: [Token] -> String
formatTokens tokens = formatTokens' $ show tokens
  where
    formatTokens' :: String -> String
    formatTokens' [] = []
    formatTokens' ('T':'o':'k':'e':'n':cs) = formatTokens' cs
    formatTokens' (c:cs)
      | c == '[' || c == ']' || c == '\"' = formatTokens' cs
      | c == ',' = '\n' : formatTokens' cs
      | otherwise = c : formatTokens' cs

