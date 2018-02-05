module TypeCheck.Unify
  ( module TypeCheck.Unify
  , module Ast.A2Constrained.Error
  , module TypeCheck.Constraint
  , module TypeCheck.Substitution
  ) where

import Control.Monad.RWS
import Data.List(sortBy)
import Data.Ord(comparing, Ordering(..))
import qualified Data.Map as M
import qualified Data.Set as S

import Ast.A2Constrained
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
  a :$= b -> unifyOne' (match ByVal a b)
  a :&= b -> unifyOne' (match ByRef a b)
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

-- TODO TODO TODO: Could I replace the status with matches that emit new constraints?
-- An incomplete match could simply re-emit itself?
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

data MatchType = ByRef | ByVal

match :: MatchType -> Type -> Type -> Match


-- Is this at all necessary? Isn't it handled by inequality?
-- match ByRef (TMut a) b = conflict WrongMutability



-- Arrays
match _ (TArray a) (TArray b) = match ByRef a b
-- match mt (TArray a) (TArray b)



-- Overloads
match mt a (TOver tvar bs) = let
  matchesByCompatibility = sortBy (comparing snd) $ zipWithResult (match mt a) bs
  firstMatch = head matchesByCompatibility

  subForError = substitution tvar TError
    -- substitute TVar results of failed overloads, to reduce "FailedToInferType" messages
    <> case a of TVar tRes -> substitution tRes TError; _ -> mempty

  in case snd firstMatch of
    (Match _ _ Complete _) -> let
      bestMatches = firstMatch :
      -- takeWhileInclusive ((==EQ) . (comparing snd) firstMatch) matchesByCompatibility?
        (takeWhile ((==EQ) . (comparing snd) firstMatch) $ tail matchesByCompatibility)

      in case bestMatches of
        [] -> conflict NoViableOverload <> subForError

        [bestMatch] -> (snd $ bestMatch) <> substitution tvar (fst bestMatch)

        matches
          -> conflict (EquallyViableOverloads a $ S.fromList $ fst <$> matches) <> subForError
    _ -> unknown

-- TVar after TOver: better to resolve overload in one place than many
match _ (TVar a) (TVar b) = if a == b then union else substitution a (TVar b)
match _ (TVar a) b = substitution a b
match _ _ (TVar _) = unknown

-- TError after TVar: need a chance to substitute TError for TVar
match _ TError _ = union
match _ _ TError = union


-- TRef after TVar: need a chance to substitute TRef for TVar
match _ (TRef a) (TRef b) = match ByRef a b

  -- Do the rules for the following cases need to be more sophisticated?
  -- Does mutability come into play?
match ByVal a (TRef b) = match ByRef a b -- Implicit dereference
match ByVal (TRef a) b = match ByRef a b -- Implicit reference

-- Mutability after TRef: implicit references must maintain mutability
match mt (TMut a) (TMut b) = match mt a b
match mt a (TMut b) = conversion <> match mt a b
match ByVal (TMut a) b = match ByVal a b

-- Functions
match mt (TFunc aPure aParams aRet) (TFunc bPure bParams bRet)
  =  (if aPure <= bPure then union else conflict $ WrongPurity aPure bPure)
  <> (if length bParams == length aParams then
        union else conflict $ WrongNumArgs (length bParams) (length aParams))
  <> (mconcat $ zipWith (match mt) bParams aParams)
  <> (match mt aRet bRet)

-- TODO: Should fall back on overload of syntactic sugar for application
match _ (TFunc _ _ (TVar res)) b = conflict (NonApplicable b) <> substitution res TError

-- I'm not sure this is needed
-- match mt a (TFunc _ _ _) = conflict $ NonApplicable b

-- Implicit conversions
match ByVal TFlt TInt = conversion

-- Equality/inequality

match _ a b = if a == b then union
  else conflict $ FailedToUnify (a :$= b)

