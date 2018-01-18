module CodeGen.Instructions
  ( module CodeGen.Instructions
  ) where

import qualified LLVM.AST as A
import qualified LLVM.AST.CallingConvention as CallingConvention
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as T

import Ast.Common.Intrinsic
import CodeGen.CodeGenM
import CodeGen.Util

intWidth = 64

int = T.IntegerType intWidth


genIntrinsic :: Intrinsic -> [A.Operand] -> CodeGenM A.Operand
genIntrinsic i ops = do
  typeAndInstruction <- intrinsicToInstruction i ops
  instructionToOperand typeAndInstruction

instructionToOperand :: (A.Type, A.Instruction) -> CodeGenM A.Operand
instructionToOperand (t, i) = instruction t i


intrinsicToInstruction :: Intrinsic -> [A.Operand] -> CodeGenM (T.Type, A.Instruction)

intrinsicToInstruction IAdd [a, b] = pure (int, A.Add
  { A.nsw = False -- no signed wrap around
  , A.nuw = False -- no unsigned wrap around
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  })

intrinsicToInstruction ISub [a, b] = pure (int, A.Sub
  { A.nsw = False
  , A.nuw = False
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  })

intrinsicToInstruction IMul [a, b] = pure (int, A.Mul
  { A.nsw = False
  , A.nuw = False
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  })

intrinsicToInstruction IDiv [a, b] = pure (int, A.SDiv
  { A.exact = False
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  })

intrinsicToInstruction IRem [a, b] = pure (int, A.SRem
  { A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  })

intrinsicToInstruction IMod [a, b] = do
  -- Mathematically correct modulus,
  -- implemented as: a mod b = ((a rem b) + b) rem b
  aRemB <- genIntrinsic IRem [a, b]
  aRemBPlusB <- genIntrinsic IAdd [aRemB, b]
  intrinsicToInstruction IRem [aRemBPlusB, b]

intrinsicToInstruction IEql [a, b] = pure (T.i1, A.ICmp IPred.EQ a b [])
intrinsicToInstruction INeq [a, b] = pure (T.i1, A.ICmp IPred.NE a b [])
intrinsicToInstruction IGrt [a, b] = pure (T.i1, A.ICmp IPred.SGT a b [])
intrinsicToInstruction ILsr [a, b] = pure (T.i1, A.ICmp IPred.SLT a b [])
intrinsicToInstruction IGeq [a, b] = pure (T.i1, A.ICmp IPred.SGE a b [])
intrinsicToInstruction ILeq [a, b] = pure (T.i1, A.ICmp IPred.SLE a b [])

intrinsicToInstruction FAdd [a, b] = pure (T.double, A.FAdd A.NoFastMathFlags a b [])
intrinsicToInstruction FSub [a, b] = pure (T.double, A.FSub A.NoFastMathFlags a b [])
intrinsicToInstruction FMul [a, b] = pure (T.double, A.FMul A.NoFastMathFlags a b [])
intrinsicToInstruction FDiv [a, b] = pure (T.double, A.FDiv A.NoFastMathFlags a b [])

intrinsicToInstruction BAnd [a, b] = pure (T.i1, A.And a b [])
intrinsicToInstruction BOrr [a, b] = pure (T.i1, A.Or a b [])

-- TODO: Ian M - you not only need to allocate it, you must fill it.
-- Unless you'd like to simplify things by splitting allocating from initializing
-- that might be wise too
intrinsicToInstruction ArrayCons [count, value] = pure (T.ptr $ T.typeOf value,
  A.Alloca (T.typeOf value) (Just count) 0 [])

intrinsicToInstruction ArrayAppImt [array, index] = do
  address <- instructionToOperand $ getElementPtr array index
  load address

intrinsicToInstruction ArrayAppMut [array, index] = do
  address <- instructionToOperand $ getElementPtr array index
  load address

intrinsicToInstruction ArrayUpdate [array, index, value] = do
  address <- instructionToOperand $ getElementPtr array index
  pure (T.void, store address value)

load :: A.Operand -> CodeGenM (A.Type, A.Instruction)
load address = pure (T.getElementType $ T.typeOf address, A.Load
  { A.volatile = False
  , A.address = address
  , A.maybeAtomicity = Nothing
  , A.alignment = 0
  , A.metadata = []
  })

store :: A.Operand -> A.Operand -> A.Instruction
store address value = A.Store
  { A.volatile = False
  , A.address = address
  , A.value = value
  , A.maybeAtomicity = Nothing
  , A.alignment = 0
  , A.metadata = []
  }

getElementPtr :: A.Operand -> A.Operand -> (A.Type, A.Instruction)
getElementPtr address index = (T.typeOf address, A.GetElementPtr
  { A.inBounds = True
  , A.address = address
  , A.indices = [index]
  , A.metadata = []
  })


-- Control flow instructions
--------------------------------------------------------------------------------
condBr :: A.Operand -> A.Name -> A.Name -> CodeGenM () -- A.Operand
condBr cond trueLabel falseLabel = setTerminator $ A.Do $
  A.CondBr cond trueLabel falseLabel []

br :: A.Name -> CodeGenM ()
br label = setTerminator $ A.Do $ A.Br label []

phi :: A.Type -> [(A.Operand, A.Name)] -> CodeGenM A.Operand
phi t pairs = instruction t $ A.Phi t pairs []

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

