module Parser.Util where

import Ast.A0Parse

eBinOp :: Expr -> Name -> Expr -> Expr
eBinOp a op b = EApp $ App (EName op) $ Args Pure [a, b]

eUnOp :: Name -> Expr -> Expr
eUnOp n a = EApp $ App (EName n) $ Args Pure [a]
