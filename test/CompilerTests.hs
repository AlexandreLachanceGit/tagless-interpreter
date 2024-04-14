{-# LANGUAGE TemplateHaskell #-}

module CompilerTests (tests) where

import Test.HUnit
import qualified Data.Text.IO as TIO

import Symantics
import Compiler
import PLInterpreter
import HaskellRepInterpreter
import LengthInterpreter
import PrettyPrintInterpreter

tests = [
    -- booleans, if-then-else
    TestCase (assertEqual "True && False" $(compile (and_ (bool True) (bool False))) False),
    TestCase (assertEqual "True && True" $(compile (and_ (bool True) (bool True))) True),
    TestCase (assertEqual "False || False" $(compile (or_ (bool False) (bool False))) False),
    TestCase (assertEqual "False || True" $(compile (or_ (bool False) (bool True))) True),
    TestCase (assertEqual "!False" $(compile (not_ (bool False))) True),
    TestCase (assertEqual "!True" $(compile (not_ (bool True))) False),
    TestCase (assertEqual "!((True || False) && False)" $(compile (not_ (and_ (or_ (bool True) (bool False)) (bool False)))) True),
    
    -- pairs and projections
    TestCase (assertEqual "(False, 2 + 1)" $(compile (pair (bool False, add (int 2) (int 1)))) (False, 3)),
    TestCase (assertEqual "fst (False, 2 + 1)" $(compile (first (pair (bool False, int 3)))) False),
    TestCase (assertEqual "snd (False, 2 + 1)" $(compile (second (pair (bool False, int 3)))) 3),

    -- lambda application
    TestCase (assertEqual "times3(4) = 12" $(compile (app (lam(\x -> mult (x) (int 3))) (int 4))) 12),
    
    -- integers, addition, multiplication, (unary) minus
    TestCase (assertEqual "1 + 2" $(compile (add (int 1) (int 2))) 3),
    TestCase (assertEqual "-2" $(compile (minus (int 2))) (-2)),
    TestCase (assertEqual "12" $(compile (mult (int 2) (int 6))) (12)),
    TestCase (assertEqual "(1 + 2) * -3" $(compile (mult (add (int 1) (int 2)) (minus (int 3)))) (-9)),
    TestCase (assertEqual "(-4 * 4) + 2" $(compile (add ((mult (minus (int 4)) (int 4))) (int 2))) (-14)),
    
    -- ordering (i.e. less than) on integers
    TestCase (assertBool "(3 < 10) && !(4 < 1)" $(compile 
        (and_ (lt (int 3) (int 10)) (not_ (lt (int 4) (int 1)))))),
    TestCase (assertBool "(3 <= 10) && !(4 <= 1) && (2 <= 2)" $(compile 
        (and_ (and_ (lte (int 3) (int 10)) (not_ (lte (int 4) (int 1)))) (lte (int 2) (int 2))))),
    TestCase (assertBool "(7 > 5) && !(2 > 7)" $(compile 
        (and_ (gt (int 7) (int 5)) (not_ (gt (int 2) (int 7)))))),
    TestCase (assertBool "(12 >= 10) && !(1 >= 5) && (2 <= 2)" $(compile 
        (and_ (and_ (gte (int 12) (int 10)) (not_ (gte (int 1) (int 5)))) (gte (int 2) (int 2))))),
    
    -- equality on integers
    TestCase (assertBool "25 == (12 + 13)" $(compile (eq (int 25) (add (int 12) (int 13))))),
    TestCase (assertBool "11 == (12 + 13)" (not $(compile (eq (int 11) (add (int 12) (int 13)))))),
    
    -- a combinator for computing fixed points 'fix'
    TestCase (assertEqual "10^7" $(compile (
            app (lam (\x -> app (app (lam
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
            ) x) (int 7))) (int 10)
        )) 
        10000000)
    ]

