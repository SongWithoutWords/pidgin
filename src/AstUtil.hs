{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}
{-# language UndecidableInstances #-}


module AstUtil where

import Ast


------------------------------------------------------------------------------------------------------------------------
class HasName a where
  name :: a -> String

instance HasName Unit where
  name unit = case unit of
    UNamespace n _ -> n
    UClass c -> name c
    UFunc f -> name f
    UVar v -> name v

instance HasName Class where
  name (Class n _) = n

instance HasName Func where
  name (Func n _) = n

instance HasName Member where
  name member = case member of
    MClass _ c -> name c
    MFunc _ _ f -> name f
    MCons _ _ -> "This"
    MVar _ v -> name v

instance HasName Var where
  name = name . typedName

instance HasName TypedName where
  name (TypedName _ n) = n


------------------------------------------------------------------------------------------------------------------------
class HasMembers a where
  members :: a -> [Member]

instance HasMembers Class where
  members (Class _ m) = m


------------------------------------------------------------------------------------------------------------------------
class HasAccess a where
  accessMod :: a -> Access

instance HasAccess Member where
  accessMod member = case member of
    MClass a _ -> a
    MFunc a _ _ -> a
    MCons a _ -> a
    MVar a _ -> a


------------------------------------------------------------------------------------------------------------------------
class HasMut a where
  mutability :: a -> Mut

instance HasMut Type where
  mutability t = case t of
    TUser m _ -> m
    TFunc {} -> Immutable
    TInferred m -> m
    TTempRef m _ -> m
    TPersRef m _ -> m
    TOption m _ -> m
    TZeroPlus _ -> Immutable
    TOnePlus _ -> Immutable
    TBln m -> m
    TChr m -> m
    TFlt m -> m
    TInt m -> m
    TNat m -> m
    TStr m -> m
    TNone -> Immutable


------------------------------------------------------------------------------------------------------------------------
class HasTypedName a where
  typedName :: a -> TypedName

instance HasTypedName Var where
  typedName (Var t _) = t


------------------------------------------------------------------------------------------------------------------------
class HasExplicitType a where
  explicitType :: a -> Type

instance HasSig a => HasExplicitType a where
  explicitType = explicitType . sig

instance {-#OVERLAPPING#-} HasExplicitType Sig where
  explicitType a = TFunc (purity a) (paramTypes a) (returnType a)

instance  {-#OVERLAPPING#-} HasExplicitType Var where
  explicitType = explicitType . typedName

instance  {-#OVERLAPPING#-} HasExplicitType TypedName where
  explicitType (TypedName t _) = t


------------------------------------------------------------------------------------------------------------------------
class HasLambda a where
  lambda :: a -> Lambda

instance HasLambda Func where
  lambda (Func _ l) = l


------------------------------------------------------------------------------------------------------------------------
class HasSig a where
  sig :: a -> Sig

instance HasLambda a => HasSig a where
  sig = sig . lambda

instance {-#OVERLAPPING#-} HasSig Lambda where
  sig (Lambda s _) = s


------------------------------------------------------------------------------------------------------------------------
class HasPurity a where
  purity :: a -> Purity

instance HasSig a => HasPurity a where
  purity = purity . sig

instance {-#OVERLAPPING#-} HasPurity Sig where
  purity (Sig p _ _) = p -- FuncType (purity a) (paramTypes a) (returnType a)


------------------------------------------------------------------------------------------------------------------------
class HasNamedParams a where
  namedParams :: a -> [TypedName]

instance HasSig a => HasNamedParams a where
  namedParams = namedParams . sig

instance {-#overlapping#-} HasNamedParams Sig where
  namedParams (Sig _ args _) = args


------------------------------------------------------------------------------------------------------------------------
class HasParamTypes a where
  paramTypes :: a -> [Type]

instance HasSig a => HasParamTypes a where
  paramTypes = paramTypes . sig

instance {-#overlapping#-} HasParamTypes Sig where
  paramTypes a = map explicitType $ namedParams a


------------------------------------------------------------------------------------------------------------------------
class HasReturnType a where
  returnType :: a -> Type

instance HasSig a => HasReturnType a where
  returnType = returnType . sig

instance {-#OVERLAPPING#-} HasReturnType Sig where
  returnType (Sig _ _ t) = t


------------------------------------------------------------------------------------------------------------------------
class HasBlock a where
  block :: a -> Block

instance HasBlock Func where
  block (Func _ l) = block l

instance HasBlock Lambda where
  block (Lambda _ b) = b

