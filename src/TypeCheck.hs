{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}

module TypeCheck(typeCheck) where

import Ast
import TypeErrors

data Result a = Result { typedResult :: a, errors :: TypeErrors } deriving(Eq, Show)

class Checked a where
  typeCheck :: a -> Result a

instance Checked Ast where
  typeCheck ast =
    let uResults = map typeCheck ast
    in Result (map typedResult uResults) (concatMap errors uResults)

instance Checked Unit where
  typeCheck u = case u of
    UNamespace name us ->
      let usChecked = map typeCheck us
      in Result (UNamespace name $ map typedResult usChecked) (concatMap errors usChecked)
    c@UClass {} -> Result c []
    f@UFunc {} -> Result f []
    UVar v ->
      let vRes = typeCheck v
      in Result (UVar $ typedResult vRes) (errors vRes)

instance Checked Var where
  typeCheck Var {} = undefined

