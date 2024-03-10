import EvalTests (tests)
import LengthTests (tests)
import PrettyPrintTests (tests)

import Test.HUnit

main :: IO ()
main = do
    _ <- runTestTT $ test (EvalTests.tests ++ LengthTests.tests ++ PrettyPrintTests.tests)
    return ()
