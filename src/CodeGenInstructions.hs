module CodeGenInstructions
  ( iadd
  , isub
  , imul
  , sdiv
  , srem
  , igreater
  , ilesser
  , igreaterEq
  , ilesserEq
  , iequal
  , fadd
  , fsub
  , phi
  , call
  , br
  , condBr
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
type IntInstruction = W.Word32 -> BinaryInstruction

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

sdiv :: IntInstruction
sdiv width a b = instruction (T.IntegerType width) $ A.SDiv False a b []

srem :: IntInstruction
srem width a b = instruction (T.IntegerType width) $ A.SRem a b []

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

condBr :: A.Operand -> A.Name -> A.Name -> CodeGenM () -- A.Operand
condBr cond trueLabel falseLabel = setTerminator $ A.Do $
  A.CondBr cond trueLabel falseLabel []
  -- undefined --op name name metadata

br :: A.Name -> CodeGenM ()
br label = setTerminator $ A.Do $ A.Br label []

phi :: A.Type -> [(A.Operand, A.Name)] -> CodeGenM A.Operand
phi t pairs = instruction t $ A.Phi t pairs []


-- and :: InstructionType
-- and t a b = instruction t $ A.And a b []

-- or :: InstructionType
-- or t a b = instruction t $ A.Or a b []


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


-- Effects
-- call :: LAst.Operand -> [LAst.Operand] -> Codegen LAst.Operand
-- call fn args = instr $ LAst.Call Nothing CallingConvention.C [] (Right fn) (toArgs args) [] []
