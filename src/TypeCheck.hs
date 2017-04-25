{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}

module TypeCheck(typeCheckAst, Result(..)) where

import Preface

import Ast
import AstUtil
import TypeErrors

import Data.List

-- TODO: I think these pattern may lend themselves to monads

data Result a = Result { output :: a, errors :: Errors } deriving(Eq, Show)

typeCheckAst :: Ast -> Result Ast

-- Knot tying implementation
-- typeCheckAst ast = let
  -- result = typeCheck typedAst ast
  -- typedAst = output result
  -- in result

-- Successive traversal implementation
typeCheckAst ast =
  let result = typeCheck ast ast
      nextAst = output result
  in if ast == nextAst then result else typeCheckAst nextAst


class Checked a where
  typeCheck :: Ast -> a -> Result a

instance Checked [Unit] where
  typeCheck ast units =
    let uResults = map (typeCheck ast) units
    in Result (map output uResults) (concatMap errors uResults)

instance Checked Unit where
  typeCheck st unit = case unit of
    UNamespace name units ->
      let unitsChkd = map (typeCheck st) units
      in Result (UNamespace name $ map output unitsChkd) (concatMap errors unitsChkd)
    c@UClass {} -> Result c [] -- TODO
    f@UFunc {} -> Result f [] -- TODO
    UVar v ->
      let vRes = typeCheck st v
      in Result (UVar $ output vRes) (errors vRes)

instance Checked Var where
  typeCheck ast v@(Var ((mutLhs, typeLhs), name) rhs) = case findType ast rhs of
    (Nothing, es) -> Result v es
    (Just typeRhs, es) -> case typeLhs of
      TInferred -> Result (Var (mutLhs & typeRhs, name) rhs) []
      _ -> Result v $ (typeLhs `assignFrom` typeRhs) ?: es

assignFrom :: Type -> Type -> Maybe Error
assignFrom a b = if a == b then Nothing else Just $ TypeConflict a b


-- take a look at the maybe monad, this appears similar

findType :: Ast -> Expr -> (Maybe Type, Errors)
findType ast e = case e of

  ELitBln _ -> found TBln
  ELitChr _ -> found TChr
  ELitFlt _ -> found TFlt
  ELitInt _ -> found TInt
  ELitStr _ -> found TStr

  EName n -> case find (\u -> n == nameOf u) ast of
    Nothing -> typeError $ UnknownId n
    Just u -> case u of
      UVar v -> found $ snd $ mTypeOf v

  EIf e1 cond e2 ->
    let
      t1 = findType ast e1
      t2 = findType ast e2
      tc = findType ast cond
    in ((fst t1), [])

  where
    found t = (Just t, [])
    foundWithErrors t es = (Just t, es)
    typeError e = (Nothing, [e])

