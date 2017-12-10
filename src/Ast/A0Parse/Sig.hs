module Ast.A0Parse.Sig where

import Ast.A0Parse.Type
import Ast.Common.Name

data Sig = Sig Purity Params (Maybe Type)
  deriving(Eq, Show)

-- In future Params could be alias for [Named MType]
type Params = [Param]

data Param = Param Mut Type Name
  deriving(Eq, Show)
