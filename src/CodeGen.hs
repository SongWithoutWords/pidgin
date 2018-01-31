{-# language OverloadedStrings #-}

module CodeGen(codeGen) where

import Control.Monad.State(gets)
import qualified Data.Map as M
import Data.String

import qualified LLVM.AST as A
import qualified LLVM.AST.Constant as C
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
    , G.basicBlocks = genBlock params block
    }

genParams :: Params -> [G.Parameter]
genParams params = map genParam params
  where
    genParam :: Param -> G.Parameter
    genParam (Named name t) = G.Parameter (typeToLlvmType t) (fromString name) []

genBlock :: Params -> Exprs -> [G.BasicBlock]
genBlock params block = buildBlocksFromCodeGenM $ genBlock' params block

genBlock' :: Params -> Exprs -> CodeGenM ()
genBlock' params exprs = do
  entryBlockName <- addBlock "entry"
  setBlock entryBlockName
  mapM_ addParamBinding params
  -- mapM_ genExpr exprs

  retOp <- genExprs exprs -- this isn't quite right... (should probaly return None for none)
  setTerminator $ A.Do $ A.Ret (Just retOp) []

  where
    addParamBinding (Named name t) = addLocalBinding name t

genExprs :: Exprs -> CodeGenM A.Operand
genExprs exprs = last <$> mapM genExpr exprs
  

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


-- Generates intermediate computations + returns a reference to the operand of the result
genExpr :: Expr -> CodeGenM A.Operand
genExpr (Expr typ expr) = case expr of

  EApp (Expr _ (EIntr i)) _ args -> do
    args' <- mapM genExpr args
    genIntrinsic i args'

  EApp e _ args -> do

    let retType = case typ of
          TFunc _ _ ret -> ret
          _ -> error "CodeGen received EApp with non applicable type"

    e' <- genExpr e
    args' <- traverse genExpr args
    call (typeToLlvmType retType) e' args'

  EName n -> do
    locals <- gets bindings
    return $ case M.lookup n locals of -- if M.member n locals
      Just op -> op
      Nothing -> globalReference n typ

  EIf ec e1 e2 -> do
    cond <- genExpr ec

    (BlockId blockNum) <- gets blockCount
    let blockName = show blockNum

    ifTrue <- addBlock $ fromString $ "if" ++ blockName
    ifFalse <- addBlock $ fromString $ "else" ++ blockName
    ifEnd <- addBlock $ fromString $ "end" ++ blockName

    condBr cond ifTrue ifFalse

    setBlock ifTrue
    e1' <- genExprs e1
    ifTrue <- gets curBlockName
    br ifEnd

    setBlock ifFalse
    e2' <- genExprs e2
    ifFalse <- gets curBlockName
    br ifEnd

    setBlock ifEnd
    phi (typeToLlvmType typ) [(e1', ifTrue), (e2', ifFalse)]

  EVal v -> case v of
    VBln b -> return $ A.ConstantOperand $ C.Int 1 $ case b of True -> 1; False -> 0
    VInt i -> return $ A.ConstantOperand $ C.Int intWidth $ toInteger i

