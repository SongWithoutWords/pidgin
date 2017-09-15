module Ast0Builder where

import Ast

namedU0Var :: Name -> Mut -> Maybe Type0 -> Expr0 -> Named Unit0
namedU0Var n m t e = Named n $ UVar $ Var0 m t e

namedU0Func :: Name -> Sig0 -> RetNotation -> Block0 -> Named Unit0
namedU0Func n s r b = Named n $ UFunc $ Func0 s r b

e0App :: Expr0 -> Args0 -> Expr0
e0App e args = Expr0 $ EApp $ App e args

e0Select :: Expr0 -> Name -> Expr0
e0Select e name = Expr0 $ ESelect $ Select e name

e0Name :: Name -> Expr0
e0Name = Expr0 . EName

e0Lambda :: Sig0 -> RetNotation -> Block0 -> Expr0
e0Lambda sig retNot block = Expr0 $ ELambda $ Func0 sig retNot block

e0Cons :: Typename -> Args0 -> Expr0
e0Cons typename args = Expr0 $ ECons typename args

e0If :: Expr0 -> Expr0 -> Expr0 -> Expr0
e0If a condition b = Expr0 $ EIf a condition b

e0UnOp :: UnOp -> Expr0 -> Expr0
e0UnOp op a = Expr0 $ EUnOp op a

e0BinOp :: BinOp -> Expr0 -> Expr0 -> Expr0
e0BinOp op a b = Expr0 $ EBinOp op a b

e0ValBln :: Bool -> Expr0
e0ValBln = Expr0 . EValBln

e0ValFlt :: Float -> Expr0
e0ValFlt = Expr0 . EValFlt

e0ValInt :: Int -> Expr0
e0ValInt = Expr0 . EValInt

e0ValStr :: String -> Expr0
e0ValStr = Expr0 . EValStr

l0App :: Expr0 -> Args0 -> LExpr0
l0App e args = LExpr0 $ LApp $ App e args

