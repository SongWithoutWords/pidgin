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


genIntrinsic :: Intrinsic -> [Maybe A.Operand] -> CodeGenM (Maybe A.Operand)
genIntrinsic i ops = do
  typeAndInstruction <- intrinsicToInstruction i ops
  instructionToOperand typeAndInstruction

instructionToOperand :: (A.Type, A.Instruction) -> CodeGenM (Maybe A.Operand)
instructionToOperand (t, i) = instruction t i


intrinsicToInstruction :: Intrinsic -> [Maybe A.Operand] -> CodeGenM (A.Type, A.Instruction)

intrinsicToInstruction IAdd [Just a, Just b] = pure $ (T.typeOf a, A.Add
  { A.nsw = False -- no signed wrap around
  , A.nuw = False -- no unsigned wrap around
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  })

intrinsicToInstruction ISub [Just a, Just b] = pure (T.typeOf a, A.Sub
  { A.nsw = False
  , A.nuw = False
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  })

intrinsicToInstruction IMul [Just a, Just b] = pure (T.typeOf a, A.Mul
  { A.nsw = False
  , A.nuw = False
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  })

intrinsicToInstruction IDiv [Just a, Just b] = pure (T.typeOf a, A.SDiv
  { A.exact = False
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  })

intrinsicToInstruction IRem [Just a, Just b] = pure (T.typeOf a, A.SRem
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

intrinsicToInstruction IEql [Just a, Just b] = pure (T.i1, A.ICmp IPred.EQ a b [])
intrinsicToInstruction INeq [Just a, Just b] = pure (T.i1, A.ICmp IPred.NE a b [])
intrinsicToInstruction IGrt [Just a, Just b] = pure (T.i1, A.ICmp IPred.SGT a b [])
intrinsicToInstruction ILsr [Just a, Just b] = pure (T.i1, A.ICmp IPred.SLT a b [])
intrinsicToInstruction IGeq [Just a, Just b] = pure (T.i1, A.ICmp IPred.SGE a b [])
intrinsicToInstruction ILeq [Just a, Just b] = pure (T.i1, A.ICmp IPred.SLE a b [])

intrinsicToInstruction FAdd [Just a, Just b] = pure (T.double, A.FAdd A.NoFastMathFlags a b [])
intrinsicToInstruction FSub [Just a, Just b] = pure (T.double, A.FSub A.NoFastMathFlags a b [])
intrinsicToInstruction FMul [Just a, Just b] = pure (T.double, A.FMul A.NoFastMathFlags a b [])
intrinsicToInstruction FDiv [Just a, Just b] = pure (T.double, A.FDiv A.NoFastMathFlags a b [])

intrinsicToInstruction BAnd [Just a, Just b] = pure (T.i1, A.And a b [])
intrinsicToInstruction BOrr [Just a, Just b] = pure (T.i1, A.Or a b [])

-- TODO: Ian M - you not only need to allocate it, you must fill it.
-- Unless you'd like to simplify things by splitting allocating from initializing
-- that might be wise too
intrinsicToInstruction ArrayCons [count, Just value] = pure (T.ptr $ T.typeOf value,
  A.Alloca (T.typeOf value) (count) 0 [])

intrinsicToInstruction ArrayAppImt [Just array, Just index] = do
  Just address <- instructionToOperand $ getElementPtr array index
  load address

intrinsicToInstruction ArrayAppMut [array, index] =
  intrinsicToInstruction ArrayAppImt [array, index]

intrinsicToInstruction ArrayUpdate [Just array, Just index, Just value] = do
  Just address <- instructionToOperand $ getElementPtr array index
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

phi :: A.Type -> [(A.Operand, A.Name)] -> CodeGenM (Maybe A.Operand)
phi t pairs = instruction t $ A.Phi t pairs []

call :: T.Type -> A.Operand -> [A.Operand] -> CodeGenM (Maybe A.Operand)
call typ op args = instruction typ $ A.Call
  { A.tailCallKind = Nothing
  , A.callingConvention = CallingConvention.Fast
  , A.returnAttributes = []
  , A.function = Right op -- Left would be inline assembly
  , A.arguments = map (\a -> (a, [])) args
  , A.functionAttributes = []
  , A.metadata = []
  }

