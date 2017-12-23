module TypeCheck.Unify
  ( unify
  , module Ast.A2Constrained.Error
  , module TypeCheck.Constraint
  , module TypeCheck.Substitution

  -- for testing
  , unifyOne
  , matchByVal
  ) where

import Control.Monad.RWS
import Data.List(sortBy)
import Data.Ord(comparing, Ordering(..))
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
  -- cs'' <- mapM applySubs cs'
  if unorderedEq cs cs' --cs''
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
      -- _ -> error $ "Constraint: " ++ show c
        -- ++ "Produced unhandled match: " ++ show m


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
    (Match errs1 dist1 stat1 _)
    (Match errs2 dist2 stat2 _)
      =  compare (length errs1) (length errs2)
      -- <> compare errs1 errs2
      <> compare dist1 dist2
      <> compare stat1 stat2
      -- <> compare subs1 subs2

betterMatch :: Match -> Match -> Ordering
betterMatch
  (Match errs1 dist1 stat1 _)
  (Match errs2 dist2 stat2 _)
    =  compare (length errs1) (length errs2)
    <> compare dist1 dist2
    <> compare stat1 stat2

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

  | TOver tvar bs <- b = let
    matchesByCompatibility = sortBy (comparing snd) $ zipWithResult (matchByVal a) bs
    firstMatch = head matchesByCompatibility
    in case snd firstMatch of
      (Match _ _ Complete _) -> let
        bestMatches = firstMatch : (takeWhile ((==EQ) . (comparing snd) firstMatch) $ tail matchesByCompatibility)
        in case bestMatches of
          [] -> conflict NoViableOverload <> substitution tvar TError
          [bestMatch] -> (snd $ bestMatch) <> substitution tvar (fst bestMatch)
          matches -> conflict (EquallyViableOverloads a $ fst <$> matches) <> substitution tvar TError
      _ -> unknown

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


  | TFunc _ _ _ <- a = conflict $ NonApplicable b

  | TFunc _ _ _ <- b = conflict $ NonApplicable a

  | otherwise = conflict $ FailedToUnify (a :$= b)


matchByRef :: Type -> Type -> Match

-- Immutable is not subtype of mutable under pass by reference
matchByRef (TMut a) (TMut b) = matchByRef a b
matchByRef a (TMut b) = matchByRef a b
matchByRef (TMut _) _ = conflict WrongMutability

