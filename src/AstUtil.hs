module AstUtil where

import Syntax

memberName :: Member -> String
memberName (MemberClass (Class name _)) = name
memberName (MemberFunction _ (Function (Signature _ name _ _) _)) = name
memberName (MemberVariable (Variable _ name _)) = name

