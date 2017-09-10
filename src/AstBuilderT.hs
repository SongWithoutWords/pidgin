{-# language GADTs #-}

module AstBuilderT where

import Ast

tApp :: TypeT -> ExprT -> ArgsC -> ExprT
tApp t e args = ExprT t $ EApp $ App e args

tName :: TypeT -> Name -> ExprT
tName t name = ExprT t $ EName name

tIf :: TypeT -> ExprT -> ExprT -> ExprT -> ExprT
tIf t a condition b = ExprT t $ EIf a condition b

tBinOp :: TypeT -> BinOp -> ExprT -> ExprT -> ExprT
tBinOp t op a b = ExprT t $ EBinOp op a b

tValBln :: Bool -> ExprT
tValBln = ExprT TBln . EValBln

tValFlt :: Float -> ExprT
tValFlt = ExprT TFlt . EValFlt

tValInt :: Int -> ExprT
tValInt = ExprT TInt . EValInt

tValStr :: String -> ExprT
tValStr = ExprT TStr . EValStr

