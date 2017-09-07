{-# language GADTs #-}
module MultiMapAst where

import Ast
-- import AstUtil
import MultiMap

multiMapAst :: AstLu -> AstMu
multiMapAst = mapUnits

mapUnits :: [UnitLu] -> Table UnitMu
mapUnits = multiFromList . map mapUnit

mapUnit :: UnitLu -> (Name, UnitMu)
mapUnit unit = case unit of
  UNamespaceL name units         -> (name, UNamespaceM $ mapUnits units)
  UClass (ClassL name members)   -> (name, UClass $ ClassM $ mapMembers members)
  UFuncL (Func name lambda)      -> (name, UFuncM $ lambda)
  UVar (VarLu mut typ name expr) -> (name, UVar $ VarMu mut typ expr)

mapMembers :: [MemberLu] -> Map Name [MemberMu]
mapMembers = multiFromList . map mapMember

mapMember :: MemberLu -> (Name, MemberMu)
mapMember member = case member of
  MClass acc (ClassL name members)  -> (name, MClass acc $ ClassM $ mapMembers $ members)
  MCons acc lambda                  -> ("This", MCons acc lambda)
  MFuncL acc mut (Func name lambda) -> (name, MFuncM acc mut lambda)
  MVar acc var                      -> (fst $ mapVar var, MVar acc $ snd $ mapVar var)

mapVar :: VarLu -> (Name, VarMu)
mapVar (VarLu mut typ name expr) = (name, VarMu mut typ expr)


