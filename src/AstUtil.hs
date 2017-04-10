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
    (UNamespace n _) -> n
    (UClass c) -> name c
    (UFunc f) -> name f
    (UVar v) -> name v

instance HasName Class where
  name (Class n _) = n

instance HasName Func where
  name (Func s _) = name s

instance HasName Sig where
  name (Sig n _) = n

instance HasName Member where
  name member = case member of
    (MClass _ c) -> name c
    (MFunc _ _ f) -> name f
    (MVar _ v) -> name v

instance HasName Var where
  name = name . typedName

instance HasName TypedName where
  name (TypedName _ n) = n


--------------------------------------------------------------------------------------------------------------------------------
class HasMembers a where
  members :: a -> [Member]

instance HasMembers Class where
  members (Class _ m) = m


--------------------------------------------------------------------------------------------------------------------------------
class HasAccess a where
  accessMod :: a -> Access

instance HasAccess Member where
  accessMod member = case member of
    (MClass a _) -> a
    (MFunc a _ _) -> a
    (MVar a _) -> a


--------------------------------------------------------------------------------------------------------------------------------
class HasMut a where
  mutability :: a -> Mut

instance HasMut Type where
  mutability t = case t of
    (TUser m _) -> m
    TFunc {} -> Immutable
    (TInferred m) -> m
    (TTempRef m _) -> m
    (TPersRef m _) -> m
    (TOption m _) -> m
    TZeroPlus {} -> Immutable
    TOnePlus  {} -> Immutable
    (TBln m) -> m
    (TChr m) -> m
    (TFlt m) -> m
    (TInt m) -> m
    (TNat m) -> m
    (TStr m) -> m


--------------------------------------------------------------------------------------------------------------------------------
class HasTypedName a where
  typedName :: a -> TypedName

instance HasTypedName Var where
  typedName (Var t _) = t


--------------------------------------------------------------------------------------------------------------------------------
class HasExplicitType a where
  explicitType :: a -> Type

instance (HasAnonSig a) => HasExplicitType a where
  explicitType = explicitType . anonSig

instance {-#OVERLAPPING#-} HasExplicitType AnonSig where
  explicitType a = TFunc (purity a) (paramTypes a) (returnType a)

instance  {-#OVERLAPPING#-} HasExplicitType Var where
  explicitType = explicitType . typedName

instance  {-#OVERLAPPING#-} HasExplicitType TypedName where
  explicitType (TypedName t _) = t


--------------------------------------------------------------------------------------------------------------------------------
-- class HasFunctionType a where
--   functionType :: a -> FuncType

-- instance (HasAnonSig a) => HasFunctionType a where
--   functionType = functionType . anonSig

-- instance {-#OVERLAPPING#-} HasFunctionType AnonSig where
--   functionType a = FuncType (purity a) (paramTypes a) (returnType a)


--------------------------------------------------------------------------------------------------------------------------------
class HasSignature a where
  signature :: a -> Sig

instance HasSignature Func where
  signature (Func s _) = s

-- Cons


--------------------------------------------------------------------------------------------------------------------------------
class HasAnonSig a where
  anonSig :: a -> AnonSig

instance HasAnonSig Func where
  anonSig = anonSig . signature

instance HasAnonSig Sig where
  anonSig (Sig _ a) = a

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

instance HasBlock Func where
  block (Func _ b) = b

instance HasBlock Lambda where
  block (Lambda _ b) = b

