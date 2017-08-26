{-# language GADTs #-}
module MultiMapAst where

import Ast
import AstUtil
import MultiMap

mapAst :: AstLu -> AstMu
mapAst = mapUnits

mapUnits :: [UnitLu] -> Table UnitMu
mapUnits = multiFromList . map mapUnit

mapUnit :: UnitLu -> (Name, UnitMu)
mapUnit u = (nameOf u, mapUnit' u)

mapUnit' :: UnitLu -> UnitMu
mapUnit' (UNamespaceL _ units) = UNamespaceM $ mapUnits units
mapUnit' (UClass c) = UClass $ ClassM $ mapMembers $ membersOf c
mapUnit' (UFuncL f) = UFuncM $ lambdaOf f
mapUnit' (UVar v) = UVar $ mapVar v

mapMembers :: [MemberLu] -> Map Name [MemberMu]
mapMembers = multiFromList . map mapMember

mapMember :: MemberLu -> (Name, MemberMu)
mapMember m = (nameOf m, mapMember' m)

mapMember' :: MemberLu -> MemberMu
mapMember' (MClass a c) = MClass a $ ClassM $ mapMembers $ membersOf c
mapMember' (MCons a l) = MCons a l
mapMember' (MFuncL a mut f) = MFuncM a mut $ lambdaOf f
mapMember' (MVar a v) = MVar a $ mapVar v

mapVar :: VarLu -> VarMu
mapVar (VarLu m t _ e) = VarMu m t e


