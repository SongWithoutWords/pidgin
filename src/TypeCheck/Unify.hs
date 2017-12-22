module TypeCheck.Unify
  ( unify
  , matchByVal
  , module Ast.A2Constrained.Error
  , module TypeCheck.Constraint
  , module TypeCheck.Substitution
  ) where

import Control.Monad.RWS
import Data.List(sort)
import qualified Data.Map as M
import qualified Data.Set as S

import Ast.A2Constrained.Error
import TypeCheck.ApplySubs
import TypeCheck.Constraint
import TypeCheck.Substitution

import Util.Preface


unify :: Constraints -> (Substitutions, Errors)
unify = execUnifyM . unifyAll


type UnifyM = RWS () Errors Substitutions

execUnifyM :: UnifyM a -> (Substitutions, Errors)
execUnifyM unifyM =
  let (substitutions, errors) = execRWS unifyM () mempty
  in (substitutions, subErrors substitutions errors)

raise :: Error -> UnifyM ()
raise = tell . S.singleton

raiseAll :: [Error] -> UnifyM ()
raiseAll = tell . S.fromList

substitute :: TVar -> Type -> UnifyM ()
substitute tvar t = modify (M.insert tvar t)

substituteAll :: Substitutions -> UnifyM ()
substituteAll = modify . M.union


unorderedEq :: (Eq a, Ord a) => [a] -> [a] -> Bool
unorderedEq xs ys = S.fromList xs == S.fromList ys

unifyAll :: [Constraint] -> UnifyM ()
unifyAll [] = pure ()
unifyAll cs = do
  cs' <- concatMapM (applySubs >=> unifyOne) cs
  modify reduceSubs
  if unorderedEq cs cs'
    then raise $ FailedUnification cs
    else unifyAll cs'

applySubs :: Constraint -> UnifyM Constraint
applySubs c = do
  subs <- get
  pure $ subConstraint subs c


unifyOne :: Constraint -> UnifyM Constraints
unifyOne c = case c of
  a :$= b -> unifyOne' (matchByVal a b)
  a :&= b -> unifyOne' (matchByRef a b)
  where
    unifyOne' :: Match -> UnifyM Constraints
    unifyOne' m = case m of
      Match errs@(_:_) _ _  subs -> raiseAll errs >> substituteAll subs >> pure []

      Match [] _ Complete subs -> substituteAll subs >> pure []

      Match [] _ Incomplete  _ -> pure [c]


-- The distance in implicit conversions
newtype Distance = Distance Word deriving(Eq, Ord, Show)

instance Monoid Distance where
  mempty = Distance 0
  mappend (Distance a) (Distance b) = Distance (a + b)

data Status
  = Complete
  | Incomplete
  deriving(Eq, Ord, Show)

instance Monoid Status where
  mempty = Complete
  mappend Complete Complete = Complete
  mappend _ _ = Incomplete

data Match
  = Match [Error] Distance Status Substitutions
  deriving(Eq, Show)

-- Ordered from best to worst match
instance Ord Match where
  compare
    (Match errs1 dist1 stat1 subs1)
    (Match errs2 dist2 stat2 subs2)
      =  compare (length errs1) (length errs2)
      <> compare errs1 errs2
      <> compare dist1 dist2
      <> compare stat1 stat2
      <> compare subs1 subs2


instance Monoid Match where
  mempty = Match [] (Distance 0) mempty mempty
  mappend
    (Match errs1 dist1 stat1 subs1)
    (Match errs2 dist2 stat2 subs2)
    = Match (errs1 <> errs2) (dist1 <> dist2) (stat1 <> stat2) (subs1 <> subs2)


union = Match [] (Distance 0) Complete mempty
substitution tvar typ = Match [] (Distance 0) Complete (M.singleton tvar typ)
unknown = Match [] (Distance 0) Incomplete mempty
conversion = Match [] (Distance 1) Complete mempty
conflict err = Match [err] (Distance 0) Complete mempty


matchByVal :: Type -> Type -> Match

-- All combinations of mutability are allowed under pass by value
matchByVal (TMut a) (TMut b) = matchByVal a b
matchByVal a (TMut b) = matchByVal a b
matchByVal (TMut a) b = matchByVal a b

-- All combinations of reference are allowed under pass by value
matchByVal (TRef a) (TRef b) = matchByRef a b
matchByVal a (TRef b) = matchByRef a b -- Implicit de-reference
matchByVal (TRef a) b = matchByRef a b -- Implicit reference


matchByVal a b

  | a == b = union

  | TVar a' <- a = substitution a' b

  | TVar _ <- b = unknown

  | TError <- a = union

  | TError <- b = union

  | TFunc aPure aParams aRet <- a
  , TFunc bPure bParams bRet <- b
    =  (if aPure <= bPure then union else conflict $ WrongPurity aPure bPure)
    <> (if length bParams == length aParams then
          union else conflict $ WrongNumArgs (length bParams) (length aParams))
    <> (mconcat $ zipWith matchByVal bParams aParams)
    <> (matchByVal aRet bRet)

  | TOver bs <- b = case bs of
      [] -> conflict NoOverloadMatchesArgs
      _ -> head $ sort $ map (matchByVal a) bs -- Choose the best match

  | TFunc _ _ _ <- a = conflict $ NonApplicable b

  | TFunc _ _ _ <- b = conflict $ NonApplicable a

  | otherwise = conflict $ FailedToUnify (a :$= b)


matchByRef :: Type -> Type -> Match

-- Immutable is not subtype of mutable under pass by reference
matchByRef (TMut a) (TMut b) = matchByRef a b
matchByRef a (TMut b) = matchByRef a b
matchByRef (TMut _) _ = conflict WrongMutability

