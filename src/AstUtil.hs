{-# language DuplicateRecordFields, FlexibleInstances, TypeSynonymInstances #-}
{-# language OverloadedLabels #-}

module AstUtil where

import Ast

class HasName a where
  nameOf :: a -> String

instance HasName Unit where
  nameOf (UnitNamespace n _) = n
  nameOf (UnitClass c) = nameOf c
  nameOf (UnitFunction f) = nameOf f
  nameOf (UnitVariable v) = nameOf v

instance HasName Class where
  nameOf = name --(Class n _) = n

instance HasName Function where
  nameOf (Function s _) = nameOf s

instance HasName Signature where
  nameOf (Signature n _) = n

instance HasName Member where
  nameOf (MemberClass _ c) = nameOf c
  nameOf (MemberFunction _ _ f) = nameOf f
  nameOf (MemberVariable _ v) = nameOf v

instance HasName Variable where
  nameOf = nameOf . typedName --(Variable typedName _) = nameOf typedName

instance HasName TypedName where
  nameOf = name


class TypeDecl a where
  typeDeclared :: a -> Type

instance TypeDecl Function where
  typeDeclared (Function s _) = typeDeclared s

instance TypeDecl Signature where
  typeDeclared = typeDeclared . anonSig

instance TypeDecl AnonSig where
  typeDeclared a = TypeFunction (purity a) (map typeDeclared $ params a) $ ret a

instance TypeDecl Variable where
  typeDeclared = typeDeclared . typedName --(Variable tn _) = typeDeclared tn

instance TypeDecl TypedName where
  typeDeclared = typ -- (TypedName t _) = t


class Returns a where
  returnType :: a -> Type

instance Returns Function where
  returnType = returnType . sig

instance Returns Signature where
  returnType = returnType . anonSig

instance Returns AnonSig where
  returnType = ret

