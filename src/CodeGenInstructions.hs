module CodeGenInstructions
  ( add
  , fadd
  , sub
  , fsub
  ) where

import qualified Data.Word as W

import qualified LLVM.AST as A
import qualified LLVM.AST.Type as T

import CodeGenM

type Instruction = A.Operand -> A.Operand -> CodeGenM A.Operand
type Instruction' a = a -> A.Operand -> A.Operand -> CodeGenM A.Operand


type IntInstruction = Instruction' W.Word32

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

add :: IntInstruction
add width = intInstruction A.Add width

sub :: IntInstruction
sub width = intInstruction A.Sub width


type FltInstruction = Instruction' T.FloatingPointType

type FltInstructionCons =
  A.FastMathFlags ->
  A.Operand ->
  A.Operand ->
  A.InstructionMetadata ->
  A.Instruction

fltInstruction :: T.FloatingPointType -> FltInstructionCons -> Instruction
fltInstruction fpType instr a b =
  instruction (T.FloatingPointType fpType) $ instr fastMathFlags a b []
  where
    fastMathFlags = A.NoFastMathFlags

fadd :: FltInstruction
fadd fpType = fltInstruction fpType A.FAdd

fsub :: FltInstruction
fsub fpType = fltInstruction fpType A.FSub

-- fadd :: FloatingPointInstruction --InstructionType T.FloatingPointType
-- fadd t a b = instruction (T.FloatingPointType t) $ A.FAdd fastMathFlags a b []

-- sub :: InstructionType W.Word32
-- sub n a b = instruction (T.IntegerType n) $ A.Sub nsw nuw a b []

-- fsub :: InstructionType
-- fsub t a b = instruction t $ A.Sub

-- -- fsub :: A.Type -> A.Operand -> 

-- and :: InstructionType
-- and t a b = instruction t $ A.And a b []

-- or :: InstructionType
-- or t a b = instruction t $ A.Or a b []

