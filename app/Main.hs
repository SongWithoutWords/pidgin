module Main where

import Lexer
import Parser
import SymTable

import FormatAst

main :: IO ()
main = do
  input <- readFile "in/input.pgn"

  let tokens = scanTokens input
  let tokenFilePath = "out/tokens.txt"
  putStrLn $ "Writing tokens to " ++ tokenFilePath
  writeFile tokenFilePath (formatTokens tokens)

  let ast = parse tokens

  let prettyAst = formatAst $ show ast
  let astFilePath = "out/ast.txt"
  putStrLn $ "Writing AST to " ++ astFilePath
  writeFile astFilePath prettyAst

  let symTable = symTableFromAst ast
  let symTablePath = "out/symtable.txt"
  putStrLn $ "Writing SymTable to " ++ symTablePath
  writeFile symTablePath $ show symTable

