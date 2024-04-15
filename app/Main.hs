{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.Text.IO as TIO

import Symantics
import PLInterpreter
import HaskellRepInterpreter
import LengthInterpreter
import PrettyPrintInterpreter
import Compiler
import TracingInterpreter
import PartialEval

-- Using example given in paper just to showcase capabilities

tpow :: Symantics repr => repr (Int -> Int -> Int)
tpow =
    lam
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

tpow7 :: Symantics repr => repr (Int -> Int)
tpow7 = lam (\x -> app (app tpow x) (int 7))

tpow7_10 :: Symantics repr => repr Int
tpow7_10 = app tpow7 (int 10)

main :: IO ()
main = do
       putStrLn "Program pretty view:"
       TIO.putStrLn (prettyView tpow7_10)
       putStrLn "\nProgram valid haskell representation:"
       TIO.putStrLn (haskellView tpow7_10)
       putStr "\nLength of program: "
       print (len tpow7_10)
       putStr "\nProgram output: "
       print (eval tpow7_10)
       putStr "\nCompiled: "
       print $(compile (
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
       -- putStr "\nTracing interpreter:\n"
       -- eval_trace (pair (int 1, bool True))
       putStr "\nPartial eval:\n"
       TIO.putStrLn (prettyView (partial_eval (add (int 1) (int 2))))
