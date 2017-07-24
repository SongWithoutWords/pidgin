{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}

module TypeCheck(typeCheckAst) where

import Preface

import Ast
import AstUtil
import qualified Ast1 as A1

import TypeErrors

import qualified Data.Map.Lazy as Map
import Data.Maybe

import Control.Monad.Writer
import Control.Monad.Trans.Reader

type ReadWriteM r w a = ReaderT r (Writer w) a
type TypeCheckM a = ReadWriteM TypeContext Errors a

runTypeCheck :: TypeContext -> TypeCheckM a -> (a, Errors)
runTypeCheck context typeCheckM = runWriter $ runReaderT typeCheckM context

raise :: Error -> TypeCheckM ()
raise e = tell [e]

found :: Monad m => a -> m (Maybe a)
found = return . Just


-- Is type b assignable to type a?
class TypeCompare a b where
  (<~) :: a -> b -> TypeCheckM ()

-- I think I can probably generalize the rhs to a functor. I think.
instance TypeCompare Type Type where
  a <~ b =  when (a /= b) $ raise TypeConflict { expected = a, received = b }

instance TypeCompare Type (Maybe Type) where
  _ <~ Nothing = return ()
  a <~ (Just b) = a <~ b

instance TypeCompare (Maybe Type) (Maybe Type) where
  Nothing <~ _ = return ()
  (Just a) <~ b = a <~ b

instance TypeCompare Type [Type] where
  typ <~ typs = mapM_ (typ<~) typs

unify :: [Type] -> Type
unify [] = TNone
unify (x:_) = x -- TODO: Do this right

-- Knot tying implementation - it's awesome!
typeCheckAst :: Ast -> (A1.Ast, Errors)
typeCheckAst ast = let
  untypedAst = A1.mapAst ast
  result@(typedAst, _) = runTypeCheck (TypeContext [] $ GlobalBindings typedAst) $ typeCheck untypedAst
  in result

class Checked a where
  typeCheck :: a -> TypeCheckM a

instance Checked A1.Ast where
  typeCheck = mapM typeCheck

instance Checked A1.Unit where
  typeCheck unit = case unit of
    A1.UNamespace n -> A1.UNamespace <$> typeCheck n
    A1.UFunc l -> A1.UFunc <$> typeCheck l
    A1.UVar v -> A1.UVar <$> typeCheck v

instance Checked Lambda where
  -- typeCheck = undefined
  typeCheck (Lambda (Sig p params mrt) b) = do
    (b', typesReturned) <- typeCheckBlock b
    case mrt of
      Nothing -> return $ Lambda (Sig p params (Just $ unify typesReturned)) b'
      Just rt -> do
        rt <~ typesReturned
        return $ Lambda (Sig p params (Just rt)) b'


-- Yields a type checked block and a list of returned types
typeCheckBlock :: Block -> TypeCheckM (Block, [Type])
typeCheckBlock [] = return ([], [])
typeCheckBlock b@[SExpr a] = do { tRet <- findType a; return (b, maybeToList tRet)}

-- TODO: Maintain a context within the block.


instance Checked A1.Var where
  typeCheck (A1.Var lMut maybeLType rhs) = do
    maybeRType <- findType rhs
    maybeLType <~ maybeRType
    return $ A1.Var lMut (maybeLType ?? maybeRType) rhs

findType :: Expr -> TypeCheckM (Maybe Type)
findType expr = do
  case expr of
    EApp app -> findTypeApp app
    EName n -> findTypeName n
    EIf e1 cond e2 -> findTypeIf e1 cond e2
    EAdd e1 e2 -> findTypeBinOp e1 e2
    ELitBln _ -> found TBln
    ELitChr _ -> found TChr
    ELitFlt _ -> found TFlt
    ELitInt _ -> found TInt
    ELitStr _ -> found TStr


findTypeApp :: App -> TypeCheckM (Maybe Type)
findTypeApp (App e params) = do
  maybeEType <- findType e
  case maybeEType of
    Nothing -> return Nothing
    Just eType -> case eType of
      TFunc purity paramTypes ret -> findTypeApp' purity paramTypes ret
      _ -> do raise $ NonApplicable eType; return Nothing

findTypeApp' :: Purity -> [Type] -> Maybe Type -> TypeCheckM (Maybe Type)
findTypeApp' _ _ = return

findTypeName :: Name -> TypeCheckM (Maybe Type)
findTypeName n = do
  env <- ask
  case Map.lookup n env of
    Nothing -> do
      raise $ UnknownId n
      return Nothing
    Just u -> case u of
      A1.UVar (A1.Var _ t _) -> return t
      A1.UFunc (Lambda s _) -> found $ typeOf s

findTypeIf :: Expr -> Expr -> Expr -> TypeCheckM (Maybe Type)
findTypeIf a cond b = do
  ta <- findType a
  tcond <- findType cond
  tb <- findType b
  -- TODO: determine common root type, e.g. typeof(a if a.exists else none) == ?A
  ta <~ tb   -- ensure that the types of the expressions are compatible
  TBln <~ tcond -- ensure the conditional expression is boolean
  return ta

-- TODO: I absolutely must generalize operatorts (hardcoding every possibility is not happening)
findTypeBinOp :: Expr -> Expr -> TypeCheckM (Maybe Type)
findTypeBinOp a b = do
  ta <- findType a
  tb <- findType b
  -- TODO: determine common root type, e.g. typeof(3 + 4.7) == TFlt
  ta <~ tb -- this is not right, but is better than nothing
  return ta


