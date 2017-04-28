{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}

module TypeCheck(typeCheckAst) where

import qualified Ast as A
import Ast1

import TypeErrors

import qualified Data.Map.Lazy as Map

import Control.Monad.Writer
import Control.Monad.Trans.Reader

type ReadWrite r w a = ReaderT r (Writer w) a

type TypeCheck a = ReadWrite Ast Errors a

runTypeCheck :: Ast -> TypeCheck a -> (a, Errors)
runTypeCheck a tc = runWriter $ runReaderT tc a

class Checked a where
  typeCheck :: a -> TypeCheck a

-- Knot tying implementation - it's amazing!
typeCheckAst :: A.Ast -> (Ast, Errors)
typeCheckAst ast = let
  untypedAst = mapAst ast
  result@(typedAst, _) = runTypeCheck typedAst $ typeCheck untypedAst
  in result

instance Checked Ast where
  typeCheck = mapM typeCheck

instance Checked Unit where
  typeCheck unit = case unit of
    UVar v -> do { vCheck <- typeCheck v; return $ UVar vCheck }

instance Checked Var where
  typeCheck v@(Var (lMut, lType) rhs) = do
    maybeRType <- findType rhs
    case maybeRType of
      Nothing -> return v
      Just rType -> case lType of
        A.TInferred -> return (Var (lMut, rType) rhs)
        _ -> do
          lType `assignFrom` rType
          return v

assignFrom :: A.Type -> A.Type -> TypeCheck ()
assignFrom a b = when (a /= b) $ tell [TypeConflict a b]

findType :: A.Expr -> TypeCheck (Maybe A.Type)
findType expr = do
  env <- ask
  case expr of

    A.ELitBln _ -> found A.TBln
    A.ELitChr _ -> found A.TChr
    A.ELitFlt _ -> found A.TFlt
    A.ELitInt _ -> found A.TInt
    A.ELitStr _ -> found A.TStr

    A.EName n -> case Map.lookup n env of
      Nothing -> do
        tell $ typeError $ UnknownId n
        return Nothing
      Just u -> case u of
        UVar (Var (m, t) e) -> found $ t

    A.EIf e1 cond e2 -> undefined
      -- do
      -- t1 <- findType e1
      -- t2 <- findType e2
      -- tc <- findType cond
      -- return $ fst t1

  where
    found = return . Just
    typeError e = [e]

