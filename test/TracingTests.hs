module TracingTests (tests) where

import Test.HUnit
import qualified Data.Text.IO as TIO

import Symantics
import TracingInterpreter


simple = (add (int 1) (int 2))
simple_e = "add operation\
\\n    create new int -> 1\
\\n    create new int -> 2\n"

if_statement = (if_ (eq (int 1) (int 1)) (add (int 3) (int 4)) (mult (int 3) (int 4)))
if_statement_e = "if statement\
\\n    equal operation\
\\n        create new int -> 1\
\\n        create new int -> 1\
\\n    add operation\
\\n        create new int -> 3\
\\n        create new int -> 4\
\\n    multiply operation\
\\n        create new int -> 3\
\\n        create new int -> 4\n"

lambda_app = (app (lam (\x -> (add (int 2) (x)))) (int 1))
lambda_app_e = "apply function\
\\n    lambda abstraction -> x0\
\\n        add operation\
\\n            create new int -> 2\
\\n            x0\
\\n    create new int -> 1\n"

complex = (if_ (bool False) (if_ (bool True) (add (first (pair (int 7, int 5))) (minus (int 2))) (int 2)) (int 0))
complex_e = "if statement\
\\n    create new bool -> False\
\\n    if statement\
\\n        create new bool -> True\
\\n        add operation\
\\n            get first of pair\
\\n                create pair\
\\n                    create new int -> 7\
\\n                    create new int -> 5\
\\n            minus operation\
\\n                create new int -> 2\
\\n        create new int -> 2\
\\n    create new int -> 0\n"

tests = [
    TestCase (assertEqual "Simple" simple_e (trace simple)),
    TestCase (assertEqual "If Statement" if_statement_e (trace if_statement)),
    TestCase (assertEqual "Lambda Application" lambda_app_e (trace lambda_app)),
    TestCase (assertEqual "Complex" complex_e (trace complex))
    ]
