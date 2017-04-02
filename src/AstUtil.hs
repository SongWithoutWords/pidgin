{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}
{-# language UndecidableInstances #-}


module AstUtil where

import Ast


--------------------------------------------------------------------------------------------------------------------------------
class HasName a where
  name :: a -> String

instance HasName Unit where
  name unit = case unit of
    (UnitNamespace n _) -> n
    (UnitClass c) -> name c
    (UnitFunction f) -> name f
    (UnitVariable v) -> name v

instance HasName Class where
  name (Class n _) = n

instance HasName Function where
  name (Function s _) = name s

instance HasName Signature where
  name (Signature n _) = n

instance HasName Member where
  name member = case member of
    (MemberClass _ c) -> name c
    (MemberFunction _ _ f) -> name f
    (MemberVariable _ v) -> name v

instance HasName Variable where
  name = name . typedName

instance HasName TypedName where
  name (TypedName _ n) = n


--------------------------------------------------------------------------------------------------------------------------------
class HasMembers a where
  members :: a -> [Member]

instance HasMembers Class where
  members (Class _ m) = m


--------------------------------------------------------------------------------------------------------------------------------
class HasAccessMod a where
  accessMod :: a -> AccessMod

instance HasAccessMod Member where
  accessMod member = case member of
    (MemberClass a _) -> a
    (MemberFunction a _ _) -> a
    (MemberVariable a _) -> a


--------------------------------------------------------------------------------------------------------------------------------
class HasMutability a where
  mutability :: a -> Mutability

instance HasMutability Type where
  mutability t = case t of
    (TypeUser m _) -> m
    TypeFunction {} -> Immutable
    (TypeInferred m) -> m
    (TypeTempRef m _) -> m
    (TypePersRef m _) -> m
    (TypeOption m _) -> m
    TypeZeroPlus {} -> Immutable
    TypeOnePlus  {} -> Immutable
    (TypePrim m _) -> m


--------------------------------------------------------------------------------------------------------------------------------
class HasTypedName a where
  typedName :: a -> TypedName

instance HasTypedName Variable where
  typedName (Variable t _) = t


--------------------------------------------------------------------------------------------------------------------------------
class HasExplicitType a where
  explicitType :: a -> Type

instance (HasAnonSig a) => HasExplicitType a where
  explicitType = explicitType . anonSig

instance {-#OVERLAPPING#-} HasExplicitType AnonSig where
  explicitType a = TypeFunction $ FunctionType (purity a) (paramTypes a) (returnType a)

instance  {-#OVERLAPPING#-} HasExplicitType Variable where
  explicitType = explicitType . typedName

instance  {-#OVERLAPPING#-} HasExplicitType TypedName where
  explicitType (TypedName t _) = t


--------------------------------------------------------------------------------------------------------------------------------
class HasFunctionType a where
  functionType :: a -> FunctionType

instance (HasAnonSig a) => HasFunctionType a where
  functionType = functionType . anonSig

instance {-#OVERLAPPING#-} HasFunctionType AnonSig where
  functionType a = FunctionType (purity a) (paramTypes a) (returnType a)


--------------------------------------------------------------------------------------------------------------------------------
class HasSignature a where
  signature :: a -> Signature

instance HasSignature Function where
  signature (Function s _) = s

-- Constructor


--------------------------------------------------------------------------------------------------------------------------------
class HasAnonSig a where
  anonSig :: a -> AnonSig

instance HasAnonSig Function where
  anonSig = anonSig . signature

instance HasAnonSig Signature where
  anonSig (Signature _ a) = a

instance HasAnonSig Lambda where
  anonSig (Lambda a _) = a


--------------------------------------------------------------------------------------------------------------------------------
class HasPurity a where
  purity :: a -> Purity

instance (HasAnonSig a) => HasPurity a where
  purity = purity . anonSig

instance {-#OVERLAPPING#-} HasPurity AnonSig where
  purity (AnonSig p _ _) = p


--------------------------------------------------------------------------------------------------------------------------------
class HasNamedParams a where
  namedParams :: a -> [TypedName]

instance (HasAnonSig a) => HasNamedParams a where
  namedParams = namedParams . anonSig

instance {-#overlapping#-} HasNamedParams AnonSig where
  namedParams (AnonSig _ args _) = args


--------------------------------------------------------------------------------------------------------------------------------
class HasParamTypes a where
  paramTypes :: a -> [Type]

instance (HasAnonSig a) => HasParamTypes a where
  paramTypes = paramTypes . anonSig

instance {-#overlapping#-} HasParamTypes AnonSig where
  paramTypes a = map explicitType $ namedParams a


--------------------------------------------------------------------------------------------------------------------------------
class HasReturnType a where
  returnType :: a -> Type

instance (HasAnonSig a) => HasReturnType a where
  returnType = returnType . anonSig

instance {-#OVERLAPPING#-} HasReturnType AnonSig where
  returnType (AnonSig _ _ t) = t


--------------------------------------------------------------------------------------------------------------------------------
class HasBlock a where
  block :: a -> Block

instance HasBlock Function where
  block (Function _ b) = b

instance HasBlock Lambda where
  block (Lambda _ b) = b

