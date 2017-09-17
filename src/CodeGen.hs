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

codeGen :: Ast2 -> A.Module
codeGen ast = A.defaultModule
  { A.moduleName = "pidgin!"
  , A.moduleDefinitions = multiMapFoldWithKey genUnit ast
  }

genUnit :: String -> Unit2 -> A.Definition
genUnit name unit = A.GlobalDefinition $ case unit of

  -- Although I think purity could be discarded earlier in compilation, it may help
  -- with validating some optimizations
  UFunc (Func1 (Sig2 purity params retType) block) -> G.functionDefaults
    { G.name = A.Name $ fromString name
    , G.parameters = let vaArgs = False in (genParams params, vaArgs)
    , G.returnType = typeToLlvmType retType
    , G.basicBlocks = genBlock params block
    }

genParams :: Params2 -> [G.Parameter]
genParams params = map genParam params
  where
    genParam :: Param2 -> G.Parameter
    genParam (Param _ typ name) = G.Parameter (typeToLlvmType typ) (nameToLlvmName name) []

genBlock :: Params2 -> Block2 -> [G.BasicBlock]
genBlock params block = buildBlocksFromCodeGenM $ genBlock' params block

genBlock' :: Params2 -> Block2 -> CodeGenM ()
genBlock' params (Block1 stmts retExpr) = do
  entryBlockName <- addBlock "entry"
  setBlock entryBlockName
  mapM_ addParamBinding params
  mapM_ genStmt stmts

  retOp <- mapM genExpr retExpr
  setTerminator $ A.Do $ A.Ret retOp []

  where
    addParamBinding (Param _ typ name) = addLocalBinding name typ

genStmt :: Stmt2 -> CodeGenM ()
genStmt stmt = case stmt of

  SVar (Named name (Var2 _ _ e)) -> do
    oper <- genExpr e
    addBinding name oper


-- Generates intermediate computations + returns a reference to the operand of the result
genExpr :: Expr2 -> CodeGenM A.Operand
genExpr (Expr2 t e) = case e of

  EName n -> return $ A.LocalReference (typeToLlvmType t) (nameToLlvmName n)

  EBinOp op a@(Expr2 ta _) b@(Expr2 tb _) -> let
    genBinOp :: BinOp -> Type2 -> Type2 -> A.Operand -> A.Operand -> CodeGenM A.Operand

    -- how does llvm handle operations between different sized ints? (not at all)

    genBinOp Add TInt TInt = add 32

    genBinOp Add TFlt TFlt = fadd T.FloatFP

    in do
      a' <- genExpr a
      b' <- genExpr b
      genBinOp op ta tb a' b'



