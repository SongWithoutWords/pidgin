{-#LANGUAGE FlexibleInstances, TypeSynonymInstances#-}
module AstUtil where

import Syntax

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

class TypeOf a where

class Typed a where
  typeOf :: a -> Type

instance Typed Function where
  typeOf (Function s _) = typeOf s

instance Typed Signature where
  typeOf (Signature _ anonSig) = typeOf anonSig

instance Typed AnonSig where
  typeOf (AnonSig purity params ret) = TypeFunction purity (map typeOf params) ret

instance Typed Variable where
  typeOf (Variable tn _) = typeOf tn

instance Typed TypedName where
  typeOf (TypedName t _) = t

