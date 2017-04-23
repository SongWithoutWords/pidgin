{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}
{-# language UndecidableInstances #-}


module AstUtil where

import Preface

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
  name = name . mTypeName

instance HasName TypedName where
  name (_, n) = n

instance HasName MTypeName where
  name (_, n) = n


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
-- class HasMut a where
--   mutability :: a -> Mut

-- instance HasMut Type where
--   mutability t = case t of
--     TUser m _ -> m
--     TFunc {} -> Immutable
--     TInferred m -> m
--     TTempRef m _ -> m
--     TPersRef m _ -> m
--     TOption m _ -> m
--     TZeroPlus _ -> Immutable
--     TOnePlus _ -> Immutable
--     TBln m -> m
--     TChr m -> m
--     TFlt m -> m
--     TInt m -> m
--     TNat m -> m
--     TStr m -> m
--     TNone -> Immutable







------------------------------------------------------------------------------------------------------------------------
class HasMTypeName a where
  mTypeName :: a -> MTypeName

instance HasMTypeName Var where
  mTypeName (Var t _) = t

instance HasMTypeName MTypeName where
  mTypeName = identity


------------------------------------------------------------------------------------------------------------------------
class IsMTypeDecl a where
  mTypeOf :: a -> MType

instance HasMTypeName a => IsMTypeDecl a where
  mTypeOf = mTypeOf . mTypeName

instance {-#OVERLAPPING#-} IsMTypeDecl MTypeName where
  mTypeOf (t, _) = t


------------------------------------------------------------------------------------------------------------------------
class IsTypeDecl a where
  typeOf :: a -> Type

-- instance {-#OVERLAPPING#-} IsMTypeDecl a => IsTypeDecl a where
  -- typeOf = snd -- . mTypeOf

instance {-#OVERLAPPING#-} HasSig a => IsTypeDecl a where
  typeOf = typeOf . sig

instance {-#OVERLAPPING#-} IsTypeDecl Sig where
  typeOf a = TFunc (purity a) (paramTypes a) (returnType a)

instance {-#OVERLAPPING#-} IsTypeDecl TypedName where
  typeOf (t, _) = t



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
  purity (Sig p _ _) = p


------------------------------------------------------------------------------------------------------------------------
class HasNamedParams a where
  namedParams :: a -> [MTypeName]

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
  paramTypes = paramTypes . namedParams

instance {-#overlapping#-} HasParamTypes [MTypeName] where
  paramTypes = map (snd . mTypeOf)


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

