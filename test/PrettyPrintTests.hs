module PrettyPrintTests (tests) where

import Test.HUnit
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Symantics
import PrettyPrintInterpreter



-- Test programs
-- booleans, if-then-else
booleansProg = and_ (not_ (bool True)) (or_ (bool True) (bool False))

-- pairs and projections
pairsProg = add (first (pair (int 2, bool True))) (second (pair (int 5, int 2)))

-- lambda, application
lambdaAppProg = app (lam(\x -> mult (x) (int 3))) (int 4)

-- integers, addition, multiplication, (unary) minus
intProg = add (minus (int 2)) (mult (int 3) (int 2))

-- ordering (i.e. less than) on integers
orderingProg = and_ (and_ (lt (int 3) (int 10)) (lte (int 4) (int 1))) (and_ (gt (int 3) (int 10)) (gte (int 4) (int 1)))

-- equality on integers
equalityProg = eq (int 25) (int 13)

-- Tests: Normal programming language interpreter
tests = [
    -- booleans, if-then-else
    TestCase (assertEqual "((not True) && (True || False))" (prettyView booleansProg) (T.pack "((not True) && (True || False))")),

    -- -- pairs and projections
    TestCase (assertEqual "((first (2,True)) + (second (5,2)))" (prettyView pairsProg) (T.pack "((first (2,True)) + (second (5,2)))")),
    
    -- lambda, application
    TestCase (assertEqual "((|x0| -> x * 3) 4)" (prettyView lambdaAppProg) (T.pack "((|x0| -> (x0 * 3)) 4)")),

    -- integers, addition, multiplication, (unary) minus
    TestCase (assertEqual "(-(2) + (3 * 2))" (prettyView intProg) (T.pack "(-(2) + (3 * 2))")),

    -- ordering (i.e. less than) on integers
    TestCase (assertEqual "(((3 < 10) && (4 <= 1)) && ((3 > 10) && (4 >= 1)))" (prettyView orderingProg) (T.pack "(((3 < 10) && (4 <= 1)) && ((3 > 10) && (4 >= 1)))")),
    
    -- equality on integers
    TestCase (assertEqual "(25 == 13)" (prettyView equalityProg) (T.pack "(25 == 13)"))
    ]

