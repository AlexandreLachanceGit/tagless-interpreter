import EvalTests (tests)
import LengthTests (tests)

import Test.HUnit

main :: IO ()
main = do
    _ <- runTestTT $ test (EvalTests.tests ++ LengthTests.tests)
    return ()
