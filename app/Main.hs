module Main where

import Lexer
import Parser

import FormatAst

main :: IO ()
main = do
  input <- readFile "in/input.pgn"
  let tokens = scanTokens input
  let tokenFilePath = "out/tokens.txt"
  putStrLn $ "Writing tokens to " ++ tokenFilePath
  writeFile tokenFilePath (show tokens)
  let rawAst = show $ parse tokens
  let prettyAst = formatAst rawAst
  let astFilePath = "out/ast.txt"
  putStrLn $ "Writing AST to " ++ astFilePath
  writeFile astFilePath prettyAst
