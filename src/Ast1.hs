module Ast1 where

-- To avoid code duplication, I think I could parameterize Ast on the type of collection it used

import qualified Ast as A
import AstUtil

import qualified Data.Map.Lazy as Map

type Table a = Map.Map String a
type Entry a = (String, a)

type Ast = Table Unit


type UnitTable = Table Unit
type MemberTable = Table Member

data Unit
  = UNamespace UnitTable
  | UClass MemberTable
  | UFunc A.Lambda
  | UVar Var
  deriving(Eq, Show)

data Member
  = MClass A.Access MemberTable
  | MFunc A.Access A.Mut A.Lambda
  | MCons A.Access
  | MVar A.Access Var
  deriving(Eq, Show)

data Var
  = Var A.MType A.Expr
  deriving(Eq, Show)


mapAst :: A.Ast -> Ast
mapAst = mapUnits

mapUnits :: [A.Unit] -> UnitTable
mapUnits = Map.fromList . map mapUnit

mapUnit :: A.Unit -> Entry Unit
mapUnit u = (nameOf u, mapUnit' u)

mapUnit' :: A.Unit -> Unit
mapUnit' (A.UNamespace _ units) = UNamespace $ mapUnits units
mapUnit' (A.UClass c) = UClass $ mapMembers $ membersOf c
mapUnit' (A.UFunc f) = UFunc $ lambdaOf f
mapUnit' (A.UVar (A.Var (mt, _) e)) = UVar $ Var mt e

mapMembers :: [A.Member] -> MemberTable
mapMembers = Map.fromList . map memberEntryFromAst

memberEntryFromAst :: A.Member -> Entry Member
memberEntryFromAst m = (nameOf m, mapMember' m)

mapMember' :: A.Member -> Member
mapMember' (A.MClass a c) = MClass a $ mapMembers $ membersOf c
mapMember' (A.MFunc a mut f) = MFunc a mut $ lambdaOf f
mapMember' (A.MVar a v) = MVar a $ mapVar v

mapVar :: A.Var -> Var
mapVar (A.Var (mt, _) e) = Var mt e



