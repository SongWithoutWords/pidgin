module X.TypeCheck.Reduce where

import Control.Monad.State
import qualified Data.IntMap.Lazy as IM
import qualified Data.Set as S

import X.Ast
import X.Error
import X.TypeCheck.Util
import Util.MultiMap

type TypeSubs = IM.IntMap Type
type ExprSubs = IM.IntMap Type

-- Watching this shape up now, I'm beginning to wonder:
--     should there be a list of expressions that are reduced, and discarded when they're
--     fully figured out, so that the size of the traversal is continuously reduced?

data ReduceState = ReduceState
  { errors :: Errors -- write-only
  -- , exprSubs :: ExprSubs
  -- , typeSubs :: TypeSubs
  }

initialState :: ReduceState
initialState = ReduceState
  { errors = S.empty
  -- , exprSubs = IM.empty
  -- , typeSubs = IM.empty
  }

type ReduceM a = State ReduceState a

runReduce :: ReduceM a -> (a, Errors)
runReduce reduce =
  let (result, ReduceState errs) = runState reduce initialState
  in (result, errs)

raise :: Error -> ReduceM ()
raise e = modify $ \s -> s{errors = S.insert e $ errors s }

reduceExpr :: Expr -> ReduceM Expr
reduceExpr (Expr typ expr) = do
  expr' <- reduceSubExprs expr
  let input' = Expr typ expr'
  let raiseAndReturn e = raise e >> pure input'
  case expr' of

    -- Select unit of namespace
    ESelect (Expr t (EName n1 [KNamespace units])) n2 _ -> do
      let newName = n1 ++ "." ++ n2
      pure $ Expr t $ EName newName $ lookupUnit n2 units

    -- Select member of struct
    ESelect e name kinds -> case typeOfExpr e of
      TData typename members -> case multiLookup name members of
        [] -> raiseAndReturn $ UnknownMemberVariable typename name
        (_:_:_) -> raiseAndReturn $ AmbigousMemberVariable typename name
        [MVar _ t] -> pure $ Expr t $ ESelect e name []
      TVar _ -> pure $ Expr typ $ ESelect e name kinds

    EApp Pure (Expr _ (EName "+" _)) [Expr _ (EVal (VInt a)), Expr _ (EVal (VInt b))]
      -> pure $ Expr TInt $ EVal $ VInt (a + b)

    e -> pure $ Expr typ e

reduceSubExprs :: Expr' -> ReduceM Expr'
reduceSubExprs expr = case expr of
  EVar n e -> liftM (EVar n) (reduceExpr e)
  ESelect e name kinds -> reduceExpr e >>= \e' -> pure $ ESelect e' name kinds
  EApp p e es -> liftM2 (EApp p) (reduceExpr e) (mapM reduceExpr es)
  e -> pure $ e

