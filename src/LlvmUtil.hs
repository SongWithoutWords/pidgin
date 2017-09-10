module LlvmUtil where

import qualified Data.ByteString.Char8 as C8

import qualified LLVM.AST as LAst
import qualified LLVM.Context as LCont
import qualified LLVM.ExecutionEngine as LExec
import qualified LLVM.Module as LMod

-- Give me an Llvm Ast + an action to run on the resulting module, and I'll do that for you :)
withModuleFromAst :: LAst.Module -> (LCont.Context -> LMod.Module -> IO a) -> IO a
withModuleFromAst astMod action = LCont.withContext $ \context ->
  LMod.withModuleFromAST context astMod $ \lmod -> action context lmod

llvmAstToAsm :: LAst.Module -> IO String
llvmAstToAsm astMod = withModuleFromAst astMod $ \_ lmod -> do
  llstr <- LMod.moduleLLVMAssembly lmod
  return $ C8.unpack llstr

