{-#LANGUAGE FlexibleInstances, TypeSynonymInstances#-}
module AstUtil where

import Ast

class Named a where
  nameOf :: a -> String

instance Named Unit where
  nameOf (UnitNamespace n _) = n
  nameOf (UnitClass c) = nameOf c
  nameOf (UnitFunction f) = nameOf f
  nameOf (UnitVariable v) = nameOf v

instance Named Class where
  nameOf (Class n _) = n

instance Named Function where
  nameOf (Function s _) = nameOf s

instance Named Signature where
  nameOf (Signature n _) = n

instance Named Member where
  nameOf (MemberClass _ c) = nameOf c
  nameOf (MemberFunction _ _ f) = nameOf f
  nameOf (MemberVariable _ v) = nameOf v

instance Named Variable where
  nameOf (Variable typedName _) = nameOf typedName

instance Named TypedName where
  nameOf (TypedName _ n) = n


class TypeDecl a where
  typeDeclared :: a -> Type

instance TypeDecl Function where
  typeDeclared (Function s _) = typeDeclared s

instance TypeDecl Signature where
  typeDeclared (Signature _ anonSig) = typeDeclared anonSig

instance TypeDecl AnonSig where
  typeDeclared (AnonSig purity params ret) = TypeFunction purity (map typeDeclared params) ret

instance TypeDecl Variable where
  typeDeclared (Variable tn _) = typeDeclared tn

instance TypeDecl TypedName where
  typeDeclared (TypedName t _) = t


class Returns a where
  returnType :: a -> Type

instance Returns Function where
  returnType (Function s _) = returnType s

instance Returns Signature where
  returnType (Signature _ anonSig) = returnType anonSig

instance Returns AnonSig where
  returnType (AnonSig _ _ t) = t

