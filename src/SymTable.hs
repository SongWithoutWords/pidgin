module SymTable(SymTable, symTableFromAst) where

import qualified Ast
import AstUtil
import qualified Data.Map.Strict as Map


type Table a = Map.Map String a
type Entry a = (String, a)

type SymTable = UnitTable

type UnitTable = Table Unit
type MemberTable = Table Member

data Unit
  = UnitNamespace UnitTable
  | UnitClass MemberTable
  | UnitLeaf Ast.Type -- May want to add accessmods to top level declarations also
  deriving(Eq, Show)

data Member
  = MemberClass Ast.AccessMod MemberTable
  | MemberFunction Ast.AccessMod Ast.Mutability Ast.Type
  | MemberVariable Ast.AccessMod Ast.Type
  deriving(Eq, Show)


symTableFromAst :: Ast.Ast -> SymTable
symTableFromAst = unitsFromAst

unitsFromAst :: [Ast.Unit] -> UnitTable
unitsFromAst = Map.fromList . map unitEntryFromAst

unitEntryFromAst :: Ast.Unit -> Entry Unit
unitEntryFromAst u = (name u, unitFromAst u)

unitFromAst :: Ast.Unit -> Unit
unitFromAst (Ast.UnitNamespace _ units) = UnitNamespace $ unitsFromAst units
unitFromAst (Ast.UnitClass c) = UnitClass $ membersFromAstClass c
unitFromAst (Ast.UnitFunction f) = UnitLeaf $ explicitType f
unitFromAst (Ast.UnitVariable v) = UnitLeaf $ explicitType v

membersFromAstClass :: Ast.Class -> MemberTable
membersFromAstClass = membersFromAst . members

membersFromAst :: [Ast.Member] -> MemberTable
membersFromAst = Map.fromList . map memberEntryFromAst

memberEntryFromAst :: Ast.Member -> Entry Member
memberEntryFromAst m = (name m, memberFromAst m)

memberFromAst :: Ast.Member -> Member
memberFromAst (Ast.MemberClass a c) = MemberClass a $ membersFromAstClass c
memberFromAst (Ast.MemberFunction a mut f) = MemberFunction a mut $ explicitType f
memberFromAst (Ast.MemberVariable a v) = MemberVariable a $ explicitType v

