{-# language GADTs #-}

module Ast2Builder where

import Ast

tApp :: Type2 -> Expr2 -> Args2 -> Expr2
tApp t e args = Expr2 t $ EApp $ App e args

tName :: Type2 -> Name -> Expr2
tName t name = Expr2 t $ EName name

tIf :: Type2 -> Expr2 -> Expr2 -> Expr2 -> Expr2
tIf t a condition b = Expr2 t $ EIf a condition b

tBinOp :: Type2 -> BinOp -> Expr2 -> Expr2 -> Expr2
tBinOp t op a b = Expr2 t $ EBinOp op a b

tValBln :: Bool -> Expr2
tValBln = Expr2 TBln . EValBln

tValFlt :: Float -> Expr2
tValFlt = Expr2 TFlt . EValFlt

tValInt :: Int -> Expr2
tValInt = Expr2 TInt . EValInt

tValStr :: String -> Expr2
tValStr = Expr2 TStr . EValStr

