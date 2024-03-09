module LengthTests (tests) where

import Test.HUnit
import qualified Data.Text.IO as TIO

import Symantics
import PLInterpreter
import HaskellRepInterpreter
import LengthInterpreter
import PrettyPrintInterpreter

-- Test programs

simpleAndFalse = and_ (bool True) (bool False)

-- Tests: Program length
tests = [
    TestCase (assertEqual "True && False" (len simpleAndFalse) 3)
    ]

