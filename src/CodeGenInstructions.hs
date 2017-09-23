module CodeGenInstructions
  ( module CodeGenInstructions
  ) where

import qualified Data.Word as W

import qualified LLVM.AST as A
import qualified LLVM.AST.CallingConvention as CallingConvention
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.Type as T

import CodeGenM

type BinaryInstruction = A.Operand -> A.Operand -> CodeGenM A.Operand


-- Integral instructions
--------------------------------------------------------------------------------
iadd :: W.Word32 -> BinaryInstruction
iadd width a b = instruction (T.IntegerType width) $ A.Add
  { A.nsw = False
  , A.nuw = False
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  }

isub :: W.Word32 -> BinaryInstruction
isub width a b = instruction (T.IntegerType width) $ A.Sub
  { A.nsw = False -- no signed wrap around
  , A.nuw = False -- no unsigned wrap around
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  }

imul :: W.Word32 -> BinaryInstruction
imul width a b = instruction (T.IntegerType width) $ A.Mul
  { A.nsw = False
  , A.nuw = False
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  }

sdiv :: W.Word32 -> BinaryInstruction
sdiv width a b = instruction (T.IntegerType width) $ A.SDiv
  { A.exact = False
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  }

srem :: W.Word32 -> BinaryInstruction
srem width a b = instruction (T.IntegerType width) $ A.SRem
  { A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  }

igreater :: BinaryInstruction
igreater a b = instruction T.i1 $ A.ICmp IPred.SGT a b []

ilesser :: BinaryInstruction
ilesser a b = instruction T.i1 $ A.ICmp IPred.SLT a b []

igreaterEq :: BinaryInstruction
igreaterEq a b = instruction T.i1 $ A.ICmp IPred.SGE a b []

ilesserEq :: BinaryInstruction
ilesserEq a b = instruction T.i1 $ A.ICmp IPred.SLE a b []

iequal :: BinaryInstruction
iequal a b = instruction T.i1 $ A.ICmp IPred.EQ a b []

inotEqual :: BinaryInstruction
inotEqual a b = instruction T.i1 $ A.ICmp IPred.NE a b []

condBr :: A.Operand -> A.Name -> A.Name -> CodeGenM () -- A.Operand
condBr cond trueLabel falseLabel = setTerminator $ A.Do $
  A.CondBr cond trueLabel falseLabel []
  -- undefined --op name name metadata

br :: A.Name -> CodeGenM ()
br label = setTerminator $ A.Do $ A.Br label []

phi :: A.Type -> [(A.Operand, A.Name)] -> CodeGenM A.Operand
phi t pairs = instruction t $ A.Phi t pairs []


-- and :: InstructionType
and t a b = instruction t $ A.And a b []

-- or :: InstructionType
or t a b = instruction t $ A.Or a b []


-- Floating point instructions
--------------------------------------------------------------------------------
type FltInstruction = T.FloatingPointType -> BinaryInstruction

type FltInstructionCons =
  A.FastMathFlags ->
  A.Operand ->
  A.Operand ->
  A.InstructionMetadata ->
  A.Instruction

fltInstruction :: T.FloatingPointType -> FltInstructionCons -> BinaryInstruction
fltInstruction fpType instr a b =
  instruction (T.FloatingPointType fpType) $ instr fastMathFlags a b []
  where
    fastMathFlags = A.NoFastMathFlags

fadd :: FltInstruction
fadd fpType = fltInstruction fpType A.FAdd

fsub :: FltInstruction
fsub fpType = fltInstruction fpType A.FSub


-- Control flow instructions
--------------------------------------------------------------------------------
call :: T.Type -> A.Operand -> [A.Operand] -> CodeGenM A.Operand
call typ op args = instruction typ $ A.Call
  { A.tailCallKind = Nothing
  , A.callingConvention = CallingConvention.C
  , A.returnAttributes = []
  , A.function = Right op -- Left would be inline assembly
  , A.arguments = map (\a -> (a, [])) args
  , A.functionAttributes = []
  , A.metadata = []
  }

