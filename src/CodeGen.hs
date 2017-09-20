{-# language OverloadedStrings #-}
{-# language GADTs #-}

module CodeGen(codeGen) where

import qualified Data.Map as M
import Data.String
import Control.Monad.State(gets)

import qualified LLVM.AST as A
import qualified LLVM.AST.Constant as C
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

intWidth = 64

-- Generates intermediate computations + returns a reference to the operand of the result
genExpr :: Expr2 -> CodeGenM A.Operand
genExpr (Expr2 t expr) = case expr of

  EApp (App e (Args _ args)) -> do

    let retType = case t of
          TFunc _ _ ret -> ret
          _ -> error "CodeGen received EApp with non applicable type"

    e' <- genExpr e
    args' <- traverse genExpr args
    call (typeToLlvmType retType) e' args'

  EName n -> do
    locals <- gets bindings
    return $ if M.member n locals
      then localReference n t
      else globalReference n t

  EUnOp op a@(Expr2 ta _) -> let

    genUnOp :: UnOp -> Type2 -> A.Operand -> CodeGenM A.Operand

    genUnOp Neg TInt = imul intWidth (A.ConstantOperand $ C.Int intWidth (-1))

    in do
      a' <- genExpr a
      genUnOp op ta a'


  EBinOp op e1@(Expr2 t1 _) e2@(Expr2 t2 _) -> let

    genBinOp :: BinOp -> Type2 -> Type2 -> A.Operand -> A.Operand -> CodeGenM A.Operand

    genBinOp Add TInt TInt = iadd intWidth
    genBinOp Sub TInt TInt = isub intWidth
    genBinOp Mul TInt TInt = imul intWidth
    genBinOp Div TInt TInt = sdiv intWidth

    genBinOp Mod TInt TInt = genMod
      where
        genMod a b = do
          -- Mathematically correct modulus, a mod b,
          -- implemented as ((a rem b) + b) rem b
          aRemB <- srem intWidth a b
          aRemBPlusB <- iadd intWidth aRemB b
          srem intWidth aRemBPlusB b

    genBinOp Greater TInt TInt = igreater intWidth
    genBinOp Lesser TInt TInt = ilesser intWidth
    genBinOp Equal TInt TInt = iequal intWidth

    genBinOp Add TFlt TFlt = fadd T.FloatFP

    in do
      e1' <- genExpr e1
      e2' <- genExpr e2
      genBinOp op t1 t2 e1' e2'

  EVal v -> case v of
    VBln b -> return $ A.ConstantOperand $ C.Int 1 $ case b of True -> 1; False -> 0
    VInt i -> return $ A.ConstantOperand $ C.Int intWidth $ toInteger i

