module Parser.Util where

import Ast.A0Parse

eBinOp :: Expr -> Name -> Expr -> Expr
eBinOp a op b = EApp (EName op) Pure [a, b]

eUnOp :: Name -> Expr -> Expr
eUnOp n a = EApp (EName n) Pure [a]
