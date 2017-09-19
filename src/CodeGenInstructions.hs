module CodeGenInstructions
  ( iadd
  , isub
  , imul
  , fadd
  , fsub
  , call
  ) where

import qualified Data.Word as W

import qualified LLVM.AST as A
import qualified LLVM.AST.CallingConvention as CallingConvention
import qualified LLVM.AST.Type as T

import CodeGenM

type BinaryInstruction = A.Operand -> A.Operand -> CodeGenM A.Operand
type BinaryInstruction' a = a -> A.Operand -> A.Operand -> CodeGenM A.Operand


-- Integral instructions
--------------------------------------------------------------------------------
type IntInstruction = BinaryInstruction' W.Word32

type IntInstructionCons =
  Bool ->
  Bool ->
  A.Operand ->
  A.Operand ->
  A.InstructionMetadata ->
  A.Instruction

intInstruction :: IntInstructionCons -> IntInstruction
intInstruction instr width a b =
  instruction (T.IntegerType width) $ instr nsw nuw a b []
  where
    nsw = False -- no signed wrap: if true signed wraps produce poison values
    nuw = False -- no unsigned wrap: if true signed wraps produce poison values

iadd :: IntInstruction
iadd width = intInstruction A.Add width

isub :: IntInstruction
isub width = intInstruction A.Sub width

imul :: IntInstruction
imul width = intInstruction A.Mul width

-- and :: InstructionType
-- and t a b = instruction t $ A.And a b []

-- or :: InstructionType
-- or t a b = instruction t $ A.Or a b []


-- Floating point instructions
--------------------------------------------------------------------------------
type FltInstruction = BinaryInstruction' T.FloatingPointType

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


-- Effects
-- call :: LAst.Operand -> [LAst.Operand] -> Codegen LAst.Operand
-- call fn args = instr $ LAst.Call Nothing CallingConvention.C [] (Right fn) (toArgs args) [] []
