{-# language OverloadedStrings #-}
module LlvmUtil where

import qualified Data.ByteString.Char8 as C8
import Foreign.Ptr(FunPtr, castFunPtr)

import qualified LLVM.Analysis as AN
import qualified LLVM.AST as A
import qualified LLVM.Context as C
import qualified LLVM.ExecutionEngine as EE
import qualified LLVM.Module as M
import qualified LLVM.PassManager as PM


foreign import ccall "dynamic" exec :: FunPtr (IO Int) -> (IO Int)

-- Llvm Ast + action to run on resulting module -> good times
withModuleFromAst :: A.Module -> (C.Context -> M.Module -> IO a) -> IO a
withModuleFromAst astMod action = C.withContext $ \context ->
  M.withModuleFromAST context astMod $ \lmod -> action context lmod

llvmAstToAsm :: A.Module -> IO String
llvmAstToAsm astMod = withModuleFromAst astMod $ \_ lmod -> do
  llstr <- M.moduleLLVMAssembly lmod
  return $ C8.unpack llstr

withJit :: C.Context -> (EE.MCJIT -> IO a) -> IO a
withJit context action = EE.withMCJIT
  context
  (Just 1) -- optimization level
  Nothing  -- model
  Nothing  -- frame pointer elimination
  Nothing  -- fast instruction selection
  action

passes :: PM.PassSetSpec
passes = PM.defaultCuratedPassSetSpec { PM.optLevel = Just 1 }

execFunction :: FunPtr a -> IO Int
execFunction fn = exec (castFunPtr fn :: FunPtr (IO Int))

execMainOfLlvmAst :: A.Module -> IO (Maybe Int)
execMainOfLlvmAst astMod = withModuleFromAst astMod $ \context lmod ->
  withJit context $ \mcjit ->
    PM.withPassManager passes $ \pm -> do
      AN.verify lmod
      _ <- PM.runPassManager pm lmod
      EE.withModuleInEngine mcjit lmod $ \ee -> do
        main <- EE.getFunction ee (A.Name "main")
        case main of
          Just f -> do
            res <- execFunction f
            return $ Just res
          Nothing -> do
            return Nothing
