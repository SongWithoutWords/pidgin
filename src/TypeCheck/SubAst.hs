module TypeCheck.SubAst
  ( subAst
  ) where

import Control.Monad.Writer
import qualified Data.Map as M

import Ast.A2Constrained as A2
import qualified Ast.A3Typed as A3
-- import TypeCheck.ApplySubs
import TypeCheck.ErrorM
import TypeCheck.Substitution
import TypeCheck.Util
import Util.MultiMap
import Util.Preface

subAst :: Substitutions -> Ast -> (A3.Ast, Errors)
subAst = runErrorM .: subAst'

subAst' :: Substitutions -> Ast -> ErrorM A3.Ast
subAst' substitutions ast = multiMapM subUnit ast

  where
    subUnit :: Unit -> ErrorM A3.Unit
    subUnit unit = case unit of
      UNamespace units -> A3.UNamespace <$> multiMapM subUnit units
      UFunc f -> A3.UFunc <$> subFunc f
      UVar v -> A3.UVar <$> subVar v

    subVar :: Var -> ErrorM A3.Var
    subVar (Var t e) = liftM2 A3.Var (subType' t) (subExpr e)

    subFunc :: Func -> ErrorM A3.Func
    subFunc (Func s b) = liftM2 A3.Func (subSig s) (subBlock b)

    subSig :: Sig -> ErrorM A3.Sig
    subSig (Sig pur params retType) =
      liftM2 (A3.Sig pur) (subParams params) (subType' retType)

    subParams :: Params -> ErrorM A3.Params
    subParams ps = mapM subParam ps

    subParam :: Param -> ErrorM A3.Param
    subParam (Named n t) = (Named n) <$> subType' t

    subBlock :: Block -> ErrorM A3.Block
    subBlock block = mapM subExpr block

    subExpr :: Expr -> ErrorM A3.Expr
    subExpr (Expr typ expr) = do
      typ' <- subType' typ

      A3.Expr typ' <$> case expr of
        EApp e p args -> do
          e' <- subExpr e
          args' <- mapM subExpr args
          pure $ A3.EApp e' p args'
        ESelect e name -> do
          e' <- subExpr e
          pure $ A3.ESelect e' name
        EName n -> pure $ A3.EName n
        EIntr i -> pure $ EIntr i
        EIf e b1 b2 ->
          liftM3 A3.EIf (subExpr e) (subBlock b1) (subBlock b2)
        ELambda f -> A3.ELambda <$> subFunc f
        EVal v -> pure $ A3.EVal v

        EOver exprs -> let
          exprsAndErrs = runErrorM . subExpr <$> exprs
          chosenExprsAndErrs = filter ((==typ') . typeOfExpr . fst) exprsAndErrs
          in do
            -- We only want to emit errors from viable overloads
            chosenExprs <- mapM (\(e, errs) -> tell errs >> pure e) chosenExprsAndErrs
            pure $ case chosenExprs of
              [Expr _ e] -> e
              overloads -> A3.EOver overloads

    subType' :: Type -> ErrorM A3.Type
    subType' typ = case typ of
      x@(TVar tvar) -> case M.lookup tvar substitutions of
        Nothing -> raise (FailedToInferType x) >> pure x
        Just y -> pure y
      TOver tvar ts -> case M.lookup tvar substitutions of
        Nothing -> do
          x <- TOver tvar <$> mapM subType' ts
          raise (FailedToInferType x) >> pure x
        Just y -> pure y
      TFunc p paramTypes retType ->
        liftM2 (A3.TFunc p) (mapM subType' paramTypes) (subType' retType)
      TMut t -> TMut <$> subType' t
      TRef t -> A3.TRef <$> subType' t
      TArray t -> A3.TArray <$> subType' t
      t -> pure t

