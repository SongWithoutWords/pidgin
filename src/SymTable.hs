module SymTable( symTableFromAst
               , SymTable
               , Unit(..)
               , Member(..)) where

import qualified Ast as A
import AstUtil
import qualified Data.Map.Strict as Map


type Table a = Map.Map String a
type Entry a = (String, a)

type SymTable = UnitTable

type UnitTable = Table Unit
type MemberTable = Table Member

data Unit
  = UNamespace UnitTable
  | UClass MemberTable
  | UnitLeaf A.Type -- May want to add accessmods to top level declarations also
  deriving(Eq, Show)

data Member
  = MClass A.Access MemberTable
  | MFunc A.Access A.Mut A.Type
  | MVar A.Access A.Type
  deriving(Eq, Show)


symTableFromAst :: A.Ast -> SymTable
symTableFromAst = unitsFromAst

unitsFromAst :: [A.Unit] -> UnitTable
unitsFromAst = Map.fromList . map unitEntryFromAst

unitEntryFromAst :: A.Unit -> Entry Unit
unitEntryFromAst u = (nameOf u, unitFromAst u)

unitFromAst :: A.Unit -> Unit
unitFromAst (A.UNamespace _ units) = UNamespace $ unitsFromAst units
unitFromAst (A.UClass c) = UClass $ membersFromAstClass c
unitFromAst (A.UFunc f) = UnitLeaf $ typeOf f
unitFromAst (A.UVar v) = UnitLeaf $ snd $ mTypeOf v

membersFromAstClass :: A.Class -> MemberTable
membersFromAstClass = membersFromAst . membersOf

membersFromAst :: [A.Member] -> MemberTable
membersFromAst = Map.fromList . map memberEntryFromAst

memberEntryFromAst :: A.Member -> Entry Member
memberEntryFromAst m = (nameOf m, memberFromAst m)

memberFromAst :: A.Member -> Member
memberFromAst (A.MClass a c) = MClass a $ membersFromAstClass c
memberFromAst (A.MFunc a mut f) = MFunc a mut $ typeOf f
memberFromAst (A.MVar a v) = MVar a $ snd $ mTypeOf v

