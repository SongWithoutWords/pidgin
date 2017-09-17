{-# language GADTs #-}

module Ast2Builder where

import Ast

e2App :: Type2 -> Expr2 -> Args2 -> Expr2
e2App t e args = Expr2 t $ EApp $ App e args

e2Name :: Type2 -> Name -> Expr2
e2Name t name = Expr2 t $ EName name

e2If :: Type2 -> Expr2 -> Expr2 -> Expr2 -> Expr2
e2If t a condition b = Expr2 t $ EIf a condition b

e2BinOp :: Type2 -> BinOp -> Expr2 -> Expr2 -> Expr2
e2BinOp t op a b = Expr2 t $ EBinOp op a b

e2ValBln :: Bool -> Expr2
e2ValBln = Expr2 TBln . EVal . VBln

e2ValFlt :: Float -> Expr2
e2ValFlt = Expr2 TFlt . EVal . VFlt

e2ValInt :: Int -> Expr2
e2ValInt = Expr2 TInt . EVal . VInt

e2ValStr :: String -> Expr2
e2ValStr = Expr2 TStr . EVal . VStr

