module Main where

import Lexer
import Parser

main :: IO ()
main = do
  input <- readFile "sample-in/sample-in.txt"
  let tokens = scanTokens input
  writeFile "sample-out/tokens.txt" (show tokens)
  print $ "tokens: " ++ show tokens
  let parseTree = pidgin tokens
  writeFile "sample-out/parse-tree.txt" (show parseTree)
  print $ "parse tree: " ++ show parseTree
