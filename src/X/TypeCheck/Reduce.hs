module X.TypeCheck.Reduce where

import Control.Monad.State
import qualified Data.IntMap.Lazy as IM
import qualified Data.Set as S

import X.Ast
import X.Error
import X.TypeCheck.Util
import Util.MultiMap

-- Watching this shape up now, I'm beginning to wonder:
--     should there be a list of expressions that are reduced, and discarded when they're
--     fully figured out, so that the size of the traversal is continuously reduced?

type TypeSubs = IM.IntMap Type
type ExprSubs = IM.IntMap Type

data Reduction = Reduction
  { errors :: Errors -- write-only
  -- , exprSubs :: ExprSubs
  -- , typeSubs :: TypeSubs
  }

initialState :: Reduction
initialState = Reduction
  { errors = S.empty
  -- , exprSubs = IM.empty
  -- , typeSubs = IM.empty
  }

type ReduceM a = State Reduction a

runReduce :: ReduceM a -> (a, Errors)
runReduce reduce =
  let (result, Reduction errs) = runState reduce initialState
  in (result, errs)

raise :: Error -> ReduceM ()
raise e = modify $ \s -> s{errors = S.insert e $ errors s }

reduceExpr :: Expr -> ReduceM Expr
reduceExpr = undefined
-- reduceExpr (Expr typ expr) = do
  -- typ' <- reduceType typ
  -- expr' <- reduceSubExprs expr
  -- reduceExpr' (Expr typ' expr')

-- reduceExpr' :: Expr -> ReduceM Expr
-- reduceExpr' (Expr typ expr) = do
--   expr' <- reduceSubExprs expr
--   let foundError err = raise err >> (pure $ Expr TError expr')
--   case expr' of

--     -- Compile time evaluation (consider checking if all args are values up-front)
--     EApp Pure (Expr _ (EName "+" _)) [Expr _ (EVal (VInt a)), Expr _ (EVal (VInt b))]
--       -> pure $ Expr TInt $ EVal $ VInt (a + b)

--     -- Function application
--     app@(EApp callPurity (Expr (TFunc purity paramTypes tRet) _) args) -> do

--       when (callPurity /= purity) $ raise $ WrongPurity purity callPurity

--       let numParams = length paramTypes; numArgs = length args
--       when (numArgs /= numParams) $ raise $ WrongNumArgs numParams numArgs

--       _ <- zipWithM constrainExpr paramTypes args

--       pure $ Expr tRet app

--     -- Non-function application
--     EApp _ (Expr t _) _ -> foundError $ NonApplicable t

--     -- If-exprs
--     eIf@(EIf a b1 b2) -> do
--       let t1 = typeOfExpr $ last b1
--       let t2 = typeOfExpr $ last b2
--       constrainExpr TBln a
--       tIf <- reduceType $ TSuper [t1, t2]
--       pure $ Expr tIf eIf

--     -- Namespace unit selection
--     ESelect (Expr t (EName n1 [UNamespace units])) n2 _ -> do
--       let newName = n1 ++ "." ++ n2
--       pure $ Expr t $ EName newName $ lookupUnit n2 units

    -- Struct member selection
    -- ESelect e name kinds -> case typeOfExpr e of
    --   TData typename members -> case multiLookup name members of
    --     [] -> foundError $ UnknownMemberVariable typename name
    --     (_:_:_) -> foundError $ AmbigousMemberVariable typename name
    --     [MVar _ t] -> pure $ Expr t $ ESelect e name []
    --   TVar _ -> pure $ Expr typ $ ESelect e name []

--     e -> pure $ Expr typ e


-- reduceSubExprs :: Expr' -> ReduceM Expr'
-- reduceSubExprs expr = case expr of
--   EApp p e es -> liftM2 (EApp p) (reduceExpr e) (mapM reduceExpr es)
--   EIf a b1 b2 -> liftM3 EIf (reduceExpr a) (mapM reduceExpr b1) (mapM reduceExpr b2)
--   ESelect e name kinds -> reduceExpr e >>= \e' -> pure $ ESelect e' name kinds
--   EVar n e -> liftM (EVar n) (reduceExpr e)
--   e -> pure $ e

-- constrainExpr :: Type -> Expr -> ReduceM ()
-- constrainExpr t1 (Expr t2 _) = constrainType t1 t2

-- reduceType :: Type -> ReduceM Type
-- -- TODO: find common super type if it exists
-- reduceType (TSuper (t:ts)) = mapM (constrainType t) ts >> pure t
-- reduceType t = pure t

-- constrainType :: Type -> Type -> ReduceM ()
-- -- TODO: Account for implicit conversions
-- constrainType t1 t2 = when (t1 /= t2) $ raise $ WrongType t1 t2


