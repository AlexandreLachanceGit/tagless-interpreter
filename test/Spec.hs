import EvalTests (tests)
import LengthTests (tests)
import PrettyPrintTests (tests)
import HaskellRepTests (tests)
import CompilerTests (tests)
import PartialEvalTests (tests)
import TracingTests (tests)

import Test.HUnit

main :: IO ()
main = do
    _ <- runTestTT $ test (
            EvalTests.tests 
            ++ LengthTests.tests 
            ++ PrettyPrintTests.tests 
            ++ HaskellRepTests.tests
            ++ CompilerTests.tests
            ++ PartialEvalTests.tests
            ++ TracingTests.tests
        )
    return ()
