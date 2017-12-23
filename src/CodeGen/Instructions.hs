module CodeGen.Instructions
  ( module CodeGen.Instructions
  ) where

import qualified LLVM.AST as A
import qualified LLVM.AST.CallingConvention as CallingConvention
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.Type as T

import Ast.Common.Intrinsic
import CodeGen.CodeGenM

intWidth = 64

int = T.IntegerType intWidth

genIntrinsic :: Intrinsic -> [A.Operand] -> CodeGenM A.Operand
genIntrinsic IAdd [a, b] = instruction int $ A.Add
  { A.nsw = False -- no signed wrap around
  , A.nuw = False -- no unsigned wrap around
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  }

genIntrinsic ISub [a, b] = instruction int $ A.Sub
  { A.nsw = False
  , A.nuw = False
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  }

genIntrinsic IMul [a, b] = instruction int $ A.Mul
  { A.nsw = False
  , A.nuw = False
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  }

genIntrinsic IDiv [a, b] = instruction int $ A.SDiv
  { A.exact = False
  , A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  }

genIntrinsic IRem [a, b] = instruction int $ A.SRem
  { A.operand0 = a
  , A.operand1 = b
  , A.metadata = []
  }

genIntrinsic IMod [a, b] = do
  -- Mathematically correct modulus,
  -- implemented as: a mod b = ((a rem b) + b) rem b
  aRemB <- genIntrinsic IRem [a, b]
  aRemBPlusB <- genIntrinsic IAdd [aRemB, b]
  genIntrinsic IRem [aRemBPlusB, b]

genIntrinsic IEql [a, b] = instruction T.i1 $ A.ICmp IPred.EQ a b []
genIntrinsic INeq [a, b] = instruction T.i1 $ A.ICmp IPred.NE a b []
genIntrinsic IGrt [a, b] = instruction T.i1 $ A.ICmp IPred.SGT a b []
genIntrinsic ILsr [a, b] = instruction T.i1 $ A.ICmp IPred.SLT a b []
genIntrinsic IGeq [a, b] = instruction T.i1 $ A.ICmp IPred.SGE a b []
genIntrinsic ILeq [a, b] = instruction T.i1 $ A.ICmp IPred.SLE a b []

genIntrinsic FAdd [a, b] = instruction T.double $ A.FAdd A.NoFastMathFlags a b []
genIntrinsic FSub [a, b] = instruction T.double $ A.FSub A.NoFastMathFlags a b []
genIntrinsic FMul [a, b] = instruction T.double $ A.FMul A.NoFastMathFlags a b []
genIntrinsic FDiv [a, b] = instruction T.double $ A.FDiv A.NoFastMathFlags a b []

genIntrinsic BAnd [a, b] = instruction T.i1 $ A.And a b []
genIntrinsic BOrr [a, b] = instruction T.i1 $ A.Or a b []


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

