module Ast.A2Constrained.PrettyPrint where

import Data.List(intercalate)

import Ast.A3Typed

-- printAst :: Ast -> String
-- printAst = concatMap printUnit

-- printNamedUnit :: Named Unit -> String
-- printNamedUnit = printUnit' 0

-- printNamedUnit' :: Int -> Named Unit -> String
-- printNamedUnit' ind unit = case unit of
--   UNamespace

printExpr :: Expr -> String
printExpr (Expr _ expr) = case expr of
  EApp e p args -> (printExpr e) ++ "(" ++ (printPurityAndArgs p args) ++ ")"
  EName n -> n
  EIntr i -> nameOfIntrinsic i
  EIf a b c -> "if" ++ printExpr a ++ "then" ++ printExprs b ++ "else" ++ printExprs c
  EVal v -> printVal v

printExprs :: Exprs -> String
printExprs [] = []
printExprs [e] = printExpr e

printPurityAndArgs :: Purity -> [Expr] -> String

printPurityAndArgs p args = intercalate ", " $ (printPurity p) ++ map printExpr args

printPurity :: Purity -> [String]
printPurity Pure = []
printPurity PRead = ["@"]
printPurity PWrite = ["~@"]

printVal :: Value -> String
printVal v = case v of
  VBln b -> if b then "true" else "false"
  VChr c -> "'" ++ show c ++ "'"
  VFlt f -> show f
  VInt i -> show i
  VStr s -> "\"" ++ show s ++ "\""

