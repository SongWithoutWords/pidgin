module SymbolTree(astToSymbolTree) where

import qualified Syntax as Syn


astToSymbolTree :: Syn.Ast -> SymbolTree
astToSymbolTree _ = []

type SymbolTree = [Unit]

data Unit
  = UnitNamespace String [Unit]
  | UnitClass Class
  | UnitFunction String Syn.Type

data Class
  = Class String [Member]

data Member
  = MemberClass Syn.AccessMod Class
  | MemberFunction Syn.AccessMod Function
  | MemberVariable Syn.AccessMod Variable

data Function
  = 

