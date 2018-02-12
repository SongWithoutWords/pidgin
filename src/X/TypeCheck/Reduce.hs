module X.TypeCheck.Reduce where

import Control.Monad.State
import qualified Data.IntMap.Lazy as IM
import qualified Data.Set as S

import X.Ast
import Ast.A2Constrained.Error
import X.TypeCheck.Util
import Util.MultiMap

type TypeSubs = IM.IntMap Type
type ExprSubs = IM.IntMap Type

-- Watching this shape up now, I'm beginning to wonder:
--     should there be a list of expressions that are reduced, and discarded when they're
--     fully figured out, so that the size of the traversal is continuously reduced?

data ReduceState = ReduceState
  { errors :: Errors -- write-only
  , exprSubs :: ExprSubs
  , typeSubs :: TypeSubs
  }

initialState :: ReduceState
initialState = ReduceState
  { errors = S.empty
  , exprSubs = IM.empty
  , typeSubs = IM.empty
  }

type ReduceM a = State ReduceState a

raise :: String -> a
raise = error

reduceExpr :: Expr -> Expr
reduceExpr (Expr typ expr) = case reduceExpr' expr of

  ESelect (Expr t (EName n1 [KNamespace units])) n2 _ -> do
    let newName = n1 ++ "." ++ n2
    Expr t $ EName newName $ lookupUnit n2 units

  ESelect e name kinds ->
    let e' = reduceExpr e
      in case typeOfExpr e' of
        TData typename members -> case multiLookup name members of
          [] -> raise $ "Type " ++ typename ++ " has no member " ++ name
          (_:_:_) -> raise $ "Type " ++ typename ++ " has no member " ++ name
          [MVar _ t] -> Expr t $ ESelect e' name []

        TVar _ -> Expr typ $ ESelect e' name kinds

  EApp (Expr _ (EName "+" _)) Pure [Expr _ (EVal (VInt a)), Expr _ (EVal (VInt b))]
    -> Expr TInt $ EVal $ VInt (a + b)

  e -> Expr typ e

reduceExpr' :: Expr' -> Expr'
reduceExpr' expr = case expr of
  ESelect e name kinds -> ESelect (reduceExpr e) name kinds
  e -> e

