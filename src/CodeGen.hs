{-# language OverloadedStrings #-}
{-# language GADTs #-}

module CodeGen(codeGen) where

-- import CodeGenM

-- import qualified Data.ByteString.Char8 as C8
-- import qualified Data.ByteString.Short as S
import Data.String

import qualified LLVM.AST as A
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Type as T

import Ast
import CodeGenInstructions
import CodeGenM
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
    , G.parameters = let vaArgs = False in (genParams params, vaArgs)
    , G.returnType = typeToLlvmType retType
    , G.basicBlocks = genBlock params block
    }

genParams :: ParamsT -> [G.Parameter]
genParams params = map genParam params
  where
    genParam :: ParamT -> G.Parameter
    genParam (Param _ typ name) = G.Parameter (typeToLlvmType typ) (nameToLlvmName name) []

-- Lets see what we can do the good old fashioned way first, may soon resort to monads

-- I'll figure it out!
genBlock :: ParamsT -> BlockC -> [G.BasicBlock] -- CodeGenM ()
genBlock params block = buildBlocksFromCodeGenM $ genBlock' params block

genBlock' :: ParamsT -> BlockC -> CodeGenM ()
genBlock' params block = do
  entryBlockName <- addBlock "entry"
  setBlock entryBlockName
  mapM_ addParamBinding params
  mapM_ genStmt block
  where
    addParamBinding (Param _ typ name) = addLocalBinding name typ

genStmt :: StmtC -> CodeGenM ()
genStmt stmt = case stmt of

  SVar (VarLc _ _ name e) -> do
    oper <- genExpr e
    addBinding name oper

  SRet e -> do
    oper <- genExpr e
    setTerminator $ A.Do $ A.Ret (Just oper) []

-- returns a sequence of temporaries and the final instruction
genExpr :: ExprT -> CodeGenM A.Operand
genExpr (ExprT t e) = case e of

  EBinOp op a@(ExprT ta _) b@(ExprT tb _) -> let
    genBinOp :: BinOp -> TypeT -> TypeT -> A.Operand -> A.Operand -> CodeGenM A.Operand

    -- how does llvm handle operations between different sized ints? (not at all)

    genBinOp Add TInt TInt = add 32

    genBinOp Add TFlt TFlt = fadd T.FloatFP

    in do
      a' <- genExpr a
      b' <- genExpr b
      genBinOp op ta tb a' b'



