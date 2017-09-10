{-# language OverloadedStrings #-}
{-# language GADTs #-}

module CodeGen(codeGen) where

-- import CodeGenM

-- import qualified Data.ByteString.Char8 as C8
-- import qualified Data.ByteString.Short as S
import Data.String

import qualified LLVM.AST as A
import qualified LLVM.AST.Global as G

import Ast
import CodeGenUtil
import MultiMap

codeGen :: AstMc -> A.Module
codeGen ast = A.defaultModule
  { A.moduleName = "pidgin!"
  , A.moduleDefinitions = multiMapFoldWithKey genUnit ast
  }

genUnit :: String -> UnitMc -> A.Definition
genUnit name unit = A.GlobalDefinition $ case unit of


  -- It is quite stupid that this "ret notation" survives all the way to code gen
  -- Although I think purity could be discarded earlier in copilation, it may help
  -- with validating some optimizations
  UFuncM (Lambda (SigC purity params retType) retNot block) -> G.functionDefaults
    { G.name = A.Name $ fromString name
    , G.parameters = let vaArgs = False in (undefined, vaArgs)
    , G.returnType = typeToLlvmType retType
    , G.basicBlocks = genBlock params block
    }

-- Lets see what we can do the good old fashioned way first, may soon resort to monads

-- I'll figure it out!
genBlock :: ParamsT -> BlockC -> [A.BasicBlock]
genBlock params block =
  [ A.BasicBlock "entry"
    (concatMap genStmt block)
    undefined
  ]

genStmt :: StmtC -> [A.Named A.Instruction]
genStmt stmt = case stmt of

  SVar (VarLc mut typ name e) ->
    let (intermediaries, instruction) = genExpr e in
    intermediaries ++ [(fromString name) A.:= instruction]


-- returns a sequence of temporaries and the final instruction
genExpr :: ExprT -> ([A.Named A.Instruction], A.Instruction)
genExpr (ExprT t e) = case e of
  EBinOp op a b -> undefined

  


