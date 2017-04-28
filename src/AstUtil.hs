{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}
{-# language UndecidableInstances #-}

module AstUtil where
import Ast


------------------------------------------------------------------------------------------------------------------------
class HasName a where
  nameOf :: a -> String

instance HasName Unit where
  nameOf unit = case unit of
    UNamespace n _ -> n
    UClass c -> nameOf c
    UFunc f -> nameOf f
    UVar v -> nameOf v

instance HasName Class where
  nameOf (Class n _) = n

instance HasName Func where
  nameOf (Func n _) = n

instance HasName Member where
  nameOf member = case member of
    MClass _ c -> nameOf c
    MFunc _ _ f -> nameOf f
    MCons _ _ -> "This"
    MVar _ v -> nameOf v

instance HasName Var where
  nameOf = nameOf . mTypeNameOf

instance HasName TypedName where
  nameOf (_, n) = n

instance HasName MTypeName where
  nameOf (_, n) = n


------------------------------------------------------------------------------------------------------------------------
class HasMembers a where
  membersOf :: a -> [Member]

instance HasMembers Class where
  membersOf (Class _ m) = m


------------------------------------------------------------------------------------------------------------------------
class HasAccess a where
  accessModOf :: a -> Access

instance HasAccess Member where
  accessModOf member = case member of
    MClass a _ -> a
    MFunc a _ _ -> a
    MCons a _ -> a
    MVar a _ -> a


------------------------------------------------------------------------------------------------------------------------
class HasMTypeName a where
  mTypeNameOf :: a -> MTypeName

instance HasMTypeName Var where
  mTypeNameOf (Var t _) = t


------------------------------------------------------------------------------------------------------------------------
class IsMTypeDecl a where
  mTypeOf :: a -> MType

instance HasMTypeName a => IsMTypeDecl a where
  mTypeOf = mTypeOf . mTypeNameOf

instance {-#OVERLAPPING#-} IsMTypeDecl MTypeName where
  mTypeOf (t, _) = t


------------------------------------------------------------------------------------------------------------------------
class IsTypeDecl a where
  typeOf :: a -> Type

-- instance {-#OVERLAPPING#-} IsMTypeDecl a => IsTypeDecl a where
--   typeOf = snd . mTypeOf

instance {-#OVERLAPPING#-} HasSig a => IsTypeDecl a where
  typeOf = typeOf . sigOf

instance {-#OVERLAPPING#-} IsTypeDecl Sig where
  typeOf a = TFunc (purityOf a) (paramTypesOf a) (returnTypeOf a)

instance {-#OVERLAPPING#-} IsTypeDecl TypedName where
  typeOf (t, _) = t


------------------------------------------------------------------------------------------------------------------------
setType :: Type -> Var -> Var
setType t (Var ((m, _), n) e) = Var ((m, t), n) e


------------------------------------------------------------------------------------------------------------------------
class HasLambda a where
  lambdaOf :: a -> Lambda

instance HasLambda Func where
  lambdaOf (Func _ l) = l


------------------------------------------------------------------------------------------------------------------------
class HasSig a where
 sigOf :: a -> Sig

instance HasLambda a => HasSig a where
 sigOf =sigOf . lambdaOf

instance {-#OVERLAPPING#-} HasSig Lambda where
 sigOf (Lambda s _) = s


------------------------------------------------------------------------------------------------------------------------
class HasPurity a where
  purityOf :: a -> Purity

instance HasSig a => HasPurity a where
  purityOf = purityOf .sigOf

instance {-#OVERLAPPING#-} HasPurity Sig where
  purityOf (Sig p _ _) = p


------------------------------------------------------------------------------------------------------------------------
class HasNamedParams a where
  namedParamsOf :: a -> [MTypeName]

instance HasSig a => HasNamedParams a where
  namedParamsOf = namedParamsOf . sigOf

instance {-#overlapping#-} HasNamedParams Sig where
  namedParamsOf (Sig _ args _) = args


------------------------------------------------------------------------------------------------------------------------
class HasParamTypes a where
  paramTypesOf :: a -> [Type]

instance HasSig a => HasParamTypes a where
  paramTypesOf = paramTypesOf . sigOf

instance {-#overlapping#-} HasParamTypes Sig where
  paramTypesOf = paramTypesOf . namedParamsOf

instance {-#overlapping#-} HasParamTypes [MTypeName] where
  paramTypesOf = map (snd . mTypeOf)


------------------------------------------------------------------------------------------------------------------------
class HasReturnType a where
  returnTypeOf :: a -> Type

instance HasSig a => HasReturnType a where
  returnTypeOf = returnTypeOf . sigOf

instance {-#OVERLAPPING#-} HasReturnType Sig where
  returnTypeOf (Sig _ _ t) = t


------------------------------------------------------------------------------------------------------------------------
class HasBlock a where
  blockOf :: a -> Block

instance HasLambda a => HasBlock a where
  blockOf = blockOf . lambdaOf

instance {-#OVERLAPPING#-} HasBlock Lambda where
  blockOf (Lambda _ b) = b

