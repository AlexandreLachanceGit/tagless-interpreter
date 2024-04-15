module PartialEvalTests (tests) where

import Test.HUnit
import qualified Data.Text.IO as TIO

import Symantics
import PartialEval 
import PrettyPrintInterpreter


simple = (add (int 1) (int 2))
simple_pe = (int 3)

if_statement = (if_ (eq (int 1) (int 1)) (add (int 3) (int 4)) (mult (int 3) (int 4)))
if_statement_pe = (int 7)

lambda_app = (app (lam (\x -> (add (int 2) (x)))) (int 1))
lambda_app_pe = (app (lam (\x -> (add (int 2) (x)))) (int 1))

tests = [
    TestCase (assertEqual "Simple" (prettyView (partial_eval simple)) (prettyView simple_pe)),
    TestCase (assertEqual "If Statement" (prettyView (partial_eval if_statement)) (prettyView if_statement_pe)),
    TestCase (assertEqual "Lambda Application" (prettyView (partial_eval lambda_app)) (prettyView lambda_app_pe)) -- limitation of symantics
    ]
