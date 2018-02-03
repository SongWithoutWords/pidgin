{-# language OverloadedStrings #-}

module CodeGen(codeGen) where

import Control.Monad.State(gets)
import qualified Data.Map as M
import Data.String

import qualified LLVM.AST as A
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Global as G

import Ast.A3Typed
import CodeGen.Instructions
import CodeGen.CodeGenM
import CodeGen.Util
import Util.MultiMap

codeGen :: Ast -> A.Module
codeGen ast = A.defaultModule
  { A.moduleName = "pidgin!"
  , A.moduleDefinitions = multiMapFoldWithKey genUnit ast
  }

genUnit :: String -> Unit -> A.Definition
genUnit name unit = A.GlobalDefinition $ case unit of

  -- Although I think purity could be discarded earlier in compilation, it may help
  -- with validating some optimizations
  UFunc (Func (Sig purity params retType) block) -> G.functionDefaults
    { G.name = A.Name $ fromString name
    , G.parameters = let vaArgs = False in (genParams params, vaArgs)
    , G.returnType = typeToLlvmType retType
    , G.basicBlocks = genFunc params block
    , G.callingConvention = CC.Fast
    }

genParams :: Params -> [G.Parameter]
genParams params = map genParam params
  where
    genParam :: Param -> G.Parameter
    genParam (Named name t) = G.Parameter (typeToLlvmType t) (fromString name) []

genFunc :: Params -> Block -> [G.BasicBlock]
genFunc params block = buildBlocksFromCodeGenM $ genFunc' params block

genFunc' :: Params -> Block -> CodeGenM ()
genFunc' params exprs = do
  entryBlockName <- addBlock "entry"
  setBlock entryBlockName
  mapM_ addParamBinding params
  -- mapM_ genExpr exprs

  retOp <- genBlock exprs -- this isn't quite right... (should probaly return None for none)
  setTerminator $ A.Do $ A.Ret retOp []

  where
    addParamBinding (Named name t) = addLocalBinding name t

genBlock :: Block -> CodeGenM (Maybe A.Operand)
genBlock exprs = last <$> mapM genExpr exprs

-- genStmt :: Stmt -> CodeGenM ()
-- genStmt stmt = case stmt of

--   SVar (Named name (Var _ e)) -> do
--     oper <- genExpr e
--     addBinding name oper

--   SExpr (Expr _ (EApp (Expr _ (EIntr ArrayUpdate)) Pure [array, index, value])) -> do
--     array' <- genExpr array
--     index' <- genExpr index
--     value' <- genExpr value
--     address <- instructionToOperand $ getElementPtr array' index'
--     action $ store address value'

--   SAssign _ _ -> undefined

genExpr' :: Expr -> CodeGenM A.Operand
genExpr' e = do
  Just e' <- genExpr e
  pure e'

-- Generates intermediate computations + returns a reference to the operand of the result
genExpr :: Expr -> CodeGenM (Maybe A.Operand)
genExpr (Expr typ expr) = case expr of

  EApp (Expr _ (EIntr i)) _ args -> do
    args' <- mapM genExpr args
    genIntrinsic i args'

  EApp e@(Expr fType _) _ args -> do

    let retType = case fType of
          TFunc _ _ ret -> ret
          _ -> error $ "CodeGen received EApp with non applicable type, in expr " ++ show (Expr typ expr)

    e' <- genExpr' e
    args' <- traverse genExpr' args
    call (typeToLlvmType retType) e' args'

  EVar (Named name (Var _ e)) -> do
    oper <- genExpr' e
    addBinding name oper
    pure Nothing

  EName n -> do
    locals <- gets bindings
    return $ Just $ case M.lookup n locals of
      Just op -> op
      Nothing -> globalReference n typ

  EIf e b1 b2 -> do
    cond <- genExpr' e

    (BlockId blockNum) <- gets blockCount
    let blockName = show blockNum

    ifTrue <- addBlock $ fromString $ "if" ++ blockName
    ifFalse <- addBlock $ fromString $ "else" ++ blockName
    ifEnd <- addBlock $ fromString $ "end" ++ blockName

    condBr cond ifTrue ifFalse

    setBlock ifTrue
    Just b1' <- genBlock b1
    ifTrue <- gets curBlockName
    br ifEnd

    setBlock ifFalse
    Just b2' <- genBlock b2
    ifFalse <- gets curBlockName
    br ifEnd

    setBlock ifEnd
    phi (typeToLlvmType typ) [(b1', ifTrue), (b2', ifFalse)]

  EVal v -> case v of
    VBln b -> return $ Just $ A.ConstantOperand $ C.Int 1 $ case b of True -> 1; False -> 0
    VInt i -> return $ Just $ A.ConstantOperand $ C.Int intWidth $ toInteger i

