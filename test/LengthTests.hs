module LengthTests (tests) where

import Test.HUnit
import qualified Data.Text.IO as TIO

import Symantics
import PLInterpreter
import HaskellRepInterpreter
import LengthInterpreter
import PrettyPrintInterpreter

-- Test programs

oneLenProg = (int 10)
simpleProg = and_ (bool True) (eq (int 5) (add (int 3) (int 2)))

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
complexProg = app tpow7 (int 10)

-- Tests: Program length
tests = [
    TestCase (assertEqual "One Length Program" (len oneLenProg) 1),
    TestCase (assertEqual "Simple Program" (len simpleProg) 7),
    TestCase (assertEqual "Complex Program" (len complexProg) 17)
    ]

