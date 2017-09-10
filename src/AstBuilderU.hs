module AstBuilderU where

import Ast

eApp :: ExprU -> ArgsU -> ExprU
eApp e args = ExprU $ EApp $ App e args

eSelect :: ExprU -> Name -> ExprU
eSelect e name = ExprU $ ESelect $ Select e name

eName :: Name -> ExprU
eName = ExprU . EName

eLambda :: SigU -> RetNotation -> BlockU -> ExprU
eLambda sig retNot block = ExprU $ ELambda $ Lambda sig retNot block

eCons :: Typename -> ArgsU -> ExprU
eCons typename args = ExprU $ ECons typename args

eIf :: ExprU -> ExprU -> ExprU -> ExprU
eIf a condition b = ExprU $ EIf a condition b

eUnOp :: UnOp -> ExprU -> ExprU
eUnOp op a = ExprU $ EUnOp op a

eBinOp :: BinOp ->  ExprU -> ExprU -> ExprU
eBinOp op a b = ExprU $ EBinOp op a b

eValBln :: Bool -> ExprU
eValBln = ExprU . EValBln

eValFlt :: Float -> ExprU
eValFlt = ExprU . EValFlt

eValInt :: Int -> ExprU
eValInt = ExprU . EValInt

eValStr :: String -> ExprU
eValStr = ExprU . EValStr

lApp :: ExprU -> ArgsU -> LExprU
lApp e args = LExprU $ LApp $ App e args

