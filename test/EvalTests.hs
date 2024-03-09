module EvalTests (tests) where

import Test.HUnit
import qualified Data.Text.IO as TIO

import Symantics
import PLInterpreter
import HaskellRepInterpreter
import LengthInterpreter
import PrettyPrintInterpreter



-- Test programs
-- booleans, if-then-else
simpleAndFalse = and_ (bool True) (bool False)
simpleAndTrue = and_ (bool True) (bool True)
simpleOrFalse = or_ (bool False) (bool False)
simpleOrTrue = or_ (bool False) (bool True)
simpleNotTrue = not_ (bool False)
simpleNotFalse = not_ (bool True)
complexBool = not_ (and_ (or_ (bool True) (bool False)) (bool False))

-- pairs and projections
simplePair = pair (bool False, add (int 2) (int 1))
simplePairFirst = first (pair (bool False, int 3))
simplePairSecond = second (pair (bool False, int 3))

-- lambda, application
times3 = lam(\x -> mult (x) (int 3))
lambdaApp = app (times3) (int 4)

-- integers, addition, multiplication, (unary) minus
simpleAdd = add (int 1) (int 2)
simpleMinus = minus (int 2)
simpleMult = mult (int 2) (int 6)
complexInt1 = mult (add (int 1) (int 2)) (minus (int 3))
complexInt2 = add ((mult (minus (int 4)) (int 4))) (int 2)

-- ordering (i.e. less than) on integers
orderingLessThan = and_ (lt (int 3) (int 10)) (not_ (lt (int 4) (int 1)))
orderingLessThanOrEqual = and_ (and_ (lte (int 3) (int 10)) (not_ (lte (int 4) (int 1)))) (lte (int 2) (int 2))
orderingGreaterThan = and_ (gt (int 7) (int 5)) (not_ (gt (int 2) (int 7)))
orderingGreaterThanOrEqual = and_ (and_ (gte (int 12) (int 10)) (not_ (gte (int 1) (int 5)))) (gte (int 2) (int 2))

-- equality on integers
equal = eq (int 25) (add (int 12) (int 13))
notEqual = eq (int 11) (add (int 12) (int 13))

-- a combinator for computing fixed points 'fix'
tpow = lam
        ( \x ->
            fix
                ( \self ->
                    lam
                        ( \n ->
                            if_
                                (lte n (int 0))
                                (int 1)
                                (mult x (app self (add n (int (-1)))))
                        )
                )
        )

tpow7 = lam (\x -> app (app tpow x) (int 7))
tpow7App = app tpow7 (int 10)

-- Tests: Normal programming language interpreter
tests = [
    -- booleans, if-then-else
    TestCase (assertEqual "True && False" (eval simpleAndFalse) False),
    TestCase (assertEqual "True && True" (eval simpleAndTrue) True),
    TestCase (assertEqual "False || False" (eval simpleOrFalse) False),
    TestCase (assertEqual "False || True" (eval simpleOrTrue) True),
    TestCase (assertEqual "!False" (eval simpleNotTrue) True),
    TestCase (assertEqual "!True" (eval simpleNotFalse) False),
    TestCase (assertEqual "!((True || False) && False)" (eval complexBool) True),

    -- pairs and projections
    TestCase (assertEqual "(False, 2 + 1)" (eval simplePair) (False, 3)),
    TestCase (assertEqual "fst (False, 2 + 1)" (eval simplePairFirst) False),
    TestCase (assertEqual "snd (False, 2 + 1)" (eval simplePairSecond) 3),

    -- pairs and projections
    TestCase (assertEqual "times3(4) = 12" (eval lambdaApp) 12),

    -- integers, addition, multiplication, (unary) minus
    TestCase (assertEqual "1 + 2" (eval simpleAdd) 3),
    TestCase (assertEqual "-2" (eval simpleMinus) (-2)),
    TestCase (assertEqual "12" (eval simpleMult) (12)),
    TestCase (assertEqual "(1 + 2) * -3" (eval complexInt1) (-9)),
    TestCase (assertEqual "(-4 * 4) + 2" (eval complexInt2) (-14)),
    
    -- ordering (i.e. less than) on integers
    TestCase (assertBool "(3 < 10) && !(4 < 1)" (eval orderingLessThan)),
    TestCase (assertBool "(3 <= 10) && !(4 <= 1) && (2 <= 2)" (eval orderingLessThanOrEqual)),
    TestCase (assertBool "(7 > 5) && !(2 > 7)" (eval orderingGreaterThan)),
    TestCase (assertBool "(12 >= 10) && !(1 >= 5) && (2 <= 2)" (eval orderingGreaterThanOrEqual)),

    -- equality on integers
    TestCase (assertBool "25 == (12 + 13)" (eval equal)),
    TestCase (assertBool "11 == (12 + 13)" (not (eval notEqual))),

    -- a combinator for computing fixed points 'fix'
    TestCase (assertEqual "10^7" (eval tpow7App) 10000000)
    ]

