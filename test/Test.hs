import qualified Test.Tasty as T

import qualified Ast.A0Parse as A0

import qualified Test.Lexer as Lexer
import qualified Test.Parser as Parser
import qualified Test.TypeCheck as TypeCheck
import qualified Test.CodeGen as CodeGen

testTimeout_μs = 20000

main :: IO ()
main = T.defaultMain tests

tests :: T.TestTree
tests = T.localOption (T.mkTimeout testTimeout_μs) $ T.testGroup "tests"
    [ Lexer.tests
    , Parser.tests
    , TypeCheck.tests
    , CodeGen.tests
    ]

