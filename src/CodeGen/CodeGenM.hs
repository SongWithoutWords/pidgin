{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}

module CodeGen.CodeGenM where

import qualified Data.ByteString.Short as S
import Data.Function
import Data.List
import qualified Data.Map as M
import Control.Monad.State

import qualified LLVM.AST as A
import qualified LLVM.AST.Global as G
-- import qualified LLVM.AST.Constant as C


-- Types
--------------------------------------------------------------------------------
type BlockTable = M.Map A.Name BlockState
type Bindings = M.Map String A.Operand

-- Newtypes may be somewhat overkill

newtype BlockId = BlockId Word
  deriving(Eq, Ord)

newtype OperandId = OperandId Word

data GenState = GenState
  { curBlockName :: A.Name
  , blocks :: BlockTable
  , blockCount :: BlockId
  , bindings :: Bindings
  , operandCount :: OperandId
  }

data BlockState = BlockState
  { index :: BlockId
  , instructions :: [A.Named A.Instruction]
  , terminator :: Maybe (A.Named A.Terminator)
  }

newGenState :: GenState
newGenState = GenState
  { curBlockName = ""
  , blocks = M.empty
  , blockCount = BlockId 0
  , bindings = M.empty
  , operandCount = OperandId 0
  }

newBlock :: BlockId -> BlockState
newBlock id = BlockState
  { index = id
  , instructions = []
  , terminator = Nothing
  }

newtype CodeGenM a = CodeGenM { runCodeGen :: State GenState a}
  deriving (Functor, Applicative, Monad, MonadState GenState)

-- GenState interface
--------------------------------------------------------------------------------

execCodeGen :: CodeGenM a -> GenState
execCodeGen gen = execState (runCodeGen gen) newGenState

buildBlocksFromCodeGenM :: CodeGenM a -> [G.BasicBlock]
buildBlocksFromCodeGenM gen =
  let genState = execCodeGen gen
  in map genBlock $ sortBlocks $ M.toList $ blocks genState
  where
    sortBlocks = sortBy (compare `on` (index . snd))
    genBlock (name, (BlockState _ instrs term)) = A.BasicBlock name (reverse instrs) $ genTerm term
      where
        genTerm (Just x) = x
        genTerm Nothing = error $ "Block " ++ show name ++ "has no terminator"


-- BlockState interface
--------------------------------------------------------------------------------

addBlock :: S.ShortByteString -> CodeGenM A.Name
addBlock name = do
  id@(BlockId i) <- gets blockCount
  let block = newBlock id
  let name' = A.Name name

  -- todo: should I ensure that there can be no duplicate block names?
  modify $ \s -> s
    { blocks = M.insert name' block $ blocks s
    , blockCount = BlockId (i + 1)
    }
  return name'

setBlock :: A.Name -> CodeGenM ()
setBlock name = modify $ \s -> s { curBlockName = name }

curBlock :: CodeGenM BlockState
curBlock = do
  name <- gets curBlockName
  blks <- gets blocks
  case M.lookup name blks of
    Just blk -> return blk
    Nothing -> error $ "No block with name " ++ show name

modifyBlock :: BlockState -> CodeGenM ()
modifyBlock new = do
  name <- gets curBlockName
  modify $ \s -> s { blocks = M.insert name new (blocks s) }

nextOperandId :: CodeGenM OperandId
nextOperandId = do
  nextId@(OperandId i) <- gets operandCount
  modify $ \s -> s { operandCount = OperandId $ i + 1 }
  return nextId

instruction :: A.Type -> A.Instruction -> CodeGenM A.Operand
instruction typ ins = do
  OperandId i <- nextOperandId
  let name = A.UnName i
  block <- curBlock
  -- let instrs = instructions block
  modifyBlock $ block { instructions = (name A.:= ins) : instructions block }
  return $ A.LocalReference typ name

setTerminator :: A.Named A.Terminator -> CodeGenM ()
setTerminator term = do
  block <- curBlock
  modifyBlock $ block { terminator = Just term }

addBinding :: String -> A.Operand -> CodeGenM ()
addBinding name oper = modify $ \s -> s { bindings = M.insert name oper $ bindings s}

