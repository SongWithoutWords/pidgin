module FormatAst (formatAst) where

indentedNewline :: Int -> String
indentedNewline n = '\n' : (replicate (2*n) ' ')

formatAst :: String -> String
formatAst s = formatAst' s 0
  where
    formatAst' :: String -> Int -> String
    formatAst' [] _ = ""
    formatAst' ('(':s) n = formatAst' s (n+1)
    formatAst' (')':s) n = formatAst' s (n-1)
    formatAst' ('[':s) n = formatAst' s (n+1)
    formatAst' (']':s) n = formatAst' s (n-1)
    formatAst' (' ':s) n = indentedNewline n ++ formatAst' s n
    formatAst' (',':s) n = indentedNewline (n-1) ++ formatAst' s n
    formatAst' (c:s) n = c : formatAst' s n

