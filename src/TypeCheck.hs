{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}

module TypeCheck(typeCheckAst) where

import Preface

import qualified Ast as A
import Ast1

import TypeErrors

import qualified Data.Map.Lazy as Map
import Data.Maybe

import Control.Monad.Writer
import Control.Monad.Trans.Reader

type ReadWrite r w a = ReaderT r (Writer w) a
type TypeCheck a = ReadWrite Ast Errors a

runTypeCheck :: Ast -> TypeCheck a -> (a, Errors)
runTypeCheck a tc = runWriter $ runReaderT tc a

raise :: Error -> TypeCheck ()
raise e = tell [e]

-- Is type b assignable to type a?
class TypeCompare a b where
  (<~) :: a -> b -> TypeCheck ()

instance TypeCompare A.Type A.Type where
  a <~ b =  when (a /= b) $ raise TypeConflict { expected = a, received = b }

instance TypeCompare A.Type (Maybe A.Type) where
  _ <~ Nothing = return ()
  a <~ (Just b) = a <~ b

instance TypeCompare (Maybe A.Type) (Maybe A.Type) where
  Nothing <~ _ = return ()
  (Just a) <~ b = a <~ b

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
    UNamespace n -> retChecked UNamespace n
    UFunc l -> retChecked UFunc l
    UVar v -> retChecked UVar v

retChecked :: Checked a => (a -> b) -> a -> TypeCheck b
retChecked cons x = do { chk <- typeCheck x; return $ cons chk}

instance Checked A.Lambda where
  typeCheck l@(A.Lambda (A.Sig p params rt) b) =
    case rt of
      Nothing -> undefined
      _ -> return l

    -- what must we do here?
       -- if l has an explicit return type, verify against its return statements
       -- if l has an implicit return type, infer return type of l
       -- enforce consistency between returns of l in any case
       -- proceed to typecheck its block (enforcing purity and so forth)


instance Checked Var where
  typeCheck (Var lMut maybeLType rhs) = do
    maybeRType <- findType rhs
    maybeLType <~ maybeRType
    return $ Var lMut (maybeLType ?? maybeRType) rhs

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
        raise $ UnknownId n
        return Nothing
      Just u -> case u of
        UVar (Var m t e) -> return t

    A.EIf e1 cond e2 -> do
      t1 <- findType e1
      t2 <- findType e2
      tc <- findType cond

      -- ensure that the types of the if and else expression are compatible
      -- TODO: determine common root type, e.g. typeof(a if a.exists else none) == ?A
      t1 <~ t2

      -- ensure that the conditional expression is boolean
      A.TBln <~ tc

      return t1

    A.EAdd e1 e2 -> do
      t1 <- findType e1
      t2 <- findType e2
      return t1

  where
    found = return . Just


