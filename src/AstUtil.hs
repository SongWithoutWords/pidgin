{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TypeSynonymInstances #-}
{-# language TypeInType #-}
{-# language UndecidableInstances #-}

module AstUtil where
import Ast

-- TODO: I feel like I really should be using lenses for this stuff


------------------------------------------------------------------------------------------------------------------------
class HasName a where
  nameOf :: a -> String

instance HasName (Unit 'SList tc) where
  nameOf unit = case unit of
    UNamespaceL n _ -> n
    UClass c -> nameOf c
    UFuncL f -> nameOf f
    UVar v -> nameOf v

instance HasName (Class 'SList tc) where
  nameOf (ClassL n _) = n

instance HasName (Func tc) where
  nameOf (Func n _) = n

instance HasName (Member 'SList tc) where
  nameOf member = case member of
    MClass _ c -> nameOf c
    MFuncL _ _ f -> nameOf f
    MCons _ _ -> "This"
    MVar _ v -> nameOf v

instance HasName (Var 'SList a) where
  nameOf (VarLu _ _ n _) = n

instance HasName Param where
  nameOf (Param _ _ n) = n


------------------------------------------------------------------------------------------------------------------------
class HasMembers a tp where
  membersOf :: a -> [Member 'SList tp]

instance HasMembers (Class 'SList tp) tp where
  membersOf (ClassL _ m) = m


------------------------------------------------------------------------------------------------------------------------
class HasAccess a where
  accessModOf :: a -> Access

instance HasAccess (Member a b) where
  accessModOf member = case member of
    MClass a _ -> a
    MFuncL a _ _ -> a
    MFuncM a _ _ -> a
    MCons a _ -> a
    MVar a _ -> a


------------------------------------------------------------------------------------------------------------------------
class IsTypeDecl a where
  typeOf :: a -> Type

instance {-#OVERLAPPING#-} HasSig a => IsTypeDecl a where
  typeOf = typeOf . sigOf

instance {-#OVERLAPPING#-} IsTypeDecl (Sig tp) where
  typeOf a = TFunc (purityOf a) (paramTypesOf a) (case returnTypeOf a of Just t -> t)

instance {-#OVERLAPPING#-} IsTypeDecl Param where
  typeOf (Param _ t _) = t


------------------------------------------------------------------------------------------------------------------------
setType :: Type -> (Var 'SList tp) -> (Var 'SList tp)
setType t (VarLu m _ n e) = VarLu m (Just t) n e


------------------------------------------------------------------------------------------------------------------------
class HasLambda a where
  lambdaOf :: a -> Lambda 'UnTyped

instance HasLambda (Func 'UnTyped) where
  lambdaOf (Func _ l) = l


------------------------------------------------------------------------------------------------------------------------
class HasSig a where
 sigOf :: a -> SigU

instance HasLambda a => HasSig a where
 sigOf = sigOf . lambdaOf

instance {-#OVERLAPPING#-} HasSig (Lambda 'UnTyped) where
 sigOf (Lambda s _ _) = s


------------------------------------------------------------------------------------------------------------------------
class HasPurity a where
  purityOf :: a -> Purity

-- instance forall a tc. HasSig a tc => HasPurity a where
  -- purityOf = purityOf .sigOf

instance HasSig a => HasPurity a where
  purityOf = purityOf .sigOf

instance {-#OVERLAPPING#-} HasPurity (Sig tp) where
  purityOf (SigU p _ _) = p


------------------------------------------------------------------------------------------------------------------------
class HasParams a where
  namedParamsOf :: a -> Params

instance HasSig a => HasParams a where
  namedParamsOf = namedParamsOf . sigOf

instance {-#overlapping#-} HasParams (Sig tp) where
  namedParamsOf (SigU _ args _) = args


------------------------------------------------------------------------------------------------------------------------
class HasParamTypes a where
  paramTypesOf :: a -> [Type]

instance HasSig a => HasParamTypes a where
  paramTypesOf = paramTypesOf . sigOf

instance {-#overlapping#-} HasParamTypes (Sig tp) where
  paramTypesOf = paramTypesOf . namedParamsOf

instance {-#overlapping#-} HasParamTypes Params where
  paramTypesOf = map typeOf


------------------------------------------------------------------------------------------------------------------------
class HasReturnType a where
  returnTypeOf :: a -> Maybe Type

instance HasSig a => HasReturnType a where
  returnTypeOf = returnTypeOf . sigOf

instance {-#OVERLAPPING#-} HasReturnType (Sig tp) where
  returnTypeOf (SigU _ _ t) = t


------------------------------------------------------------------------------------------------------------------------
class HasBlock a where
  blockOf :: a -> BlockU

instance HasLambda a => HasBlock a where
  blockOf = blockOf . lambdaOf

instance {-#OVERLAPPING#-} HasBlock LambdaU where
  blockOf (Lambda _ _ b) = b

