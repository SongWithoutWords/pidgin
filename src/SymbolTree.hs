module SymbolTree(SymTable, astToSymTable) where

import qualified Syntax as Ast

import qualified Data.Map.Strict as Map


-- Types

type Table a = Map.Map String a
type Entry a = (String, a)

type SymTable = Table Unit

data Unit
  = UnitNamespace Namespace
  | UnitClass Class
  | UnitFunction Function
  deriving(Eq, Show)

type Namespace = Table Unit
type Class = Table (Ast.AccessMod Member)

-- Access-qualified member
data QMember
  = QMember Ast.AccessMod Member

data Member
  = MemberClass Class
  | MemberFunction Function
  | MemberVariable Ast.AccessMod Ast.Type
  deriving(Eq, Show)

data Function
  = Function Ast.Purity [Ast.Type] Ast.Type
  deriving(Eq, Show)


-- Construction

astToSymTable :: Ast.Root -> SymTable
astToSymTable = unitsToTable

unitsToTable :: [Ast.Unit] -> Table Unit
unitsToTable = Map.fromList . (map unitToEntry)

unitToEntry :: Ast.Unit -> Entry Unit
unitToEntry (Ast.UnitNamespace name units) = (name, Namespace $ unitsToTable units)
unitToEntry (Ast.UnitClass c) = classToEntry c
unitToEntry (Ast.UnitFunction f) = functionToEntry f

classToEntry :: Ast.Class -> Entry Unit
classToEntry (Ast.Class name members) = (name, Class $ membersToTable members)

membersToTable :: [Ast.Member] -> Table QMember
membersToTable = Map.fromList . (map memberToEntry)

memberToEntry :: Ast.Member -> Entry QMember
memberToEntry (access m) = (umemberName m, (access, umemberTo))

umemberName :: Ast.UMember -> String



functionToEntry :: Ast.Function -> Entry


