module Ast0Builder where

import Ast

uVar :: 

eApp :: Expr0 -> Args0 -> Expr0
eApp e args = Expr0 $ EApp $ App e args

eSelect :: Expr0 -> Name -> Expr0
eSelect e name = Expr0 $ ESelect $ Select e name

eName :: Name -> Expr0
eName = Expr0 . EName

eLambda :: Sig0 -> RetNotation -> Block0 -> Expr0
eLambda sig retNot block = Expr0 $ ELambda $ Func0 sig retNot block

eCons :: Typename -> Args0 -> Expr0
eCons typename args = Expr0 $ ECons typename args

eIf :: Expr0 -> Expr0 -> Expr0 -> Expr0
eIf a condition b = Expr0 $ EIf a condition b

eUnOp :: UnOp -> Expr0 -> Expr0
eUnOp op a = Expr0 $ EUnOp op a

eBinOp :: BinOp -> Expr0 -> Expr0 -> Expr0
eBinOp op a b = Expr0 $ EBinOp op a b

eValBln :: Bool -> Expr0
eValBln = Expr0 . EValBln

eValFlt :: Float -> Expr0
eValFlt = Expr0 . EValFlt

eValInt :: Int -> Expr0
eValInt = Expr0 . EValInt

eValStr :: String -> Expr0
eValStr = Expr0 . EValStr

lApp :: Expr0 -> Args0 -> LExpr0
lApp e args = LExpr0 $ LApp $ App e args

