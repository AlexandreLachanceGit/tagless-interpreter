{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import qualified Data.Text.IO as TIO

import Symantics
import PLInterpreter
import HaskellRepInterpreter
import LengthInterpreter
import PrettyPrintInterpreter

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
       putStrLn "Program valid haskell representation:"
       TIO.putStrLn (haskellView tpow7_10)
       putStrLn "Length of program:"
       print (len tpow7_10)
       putStrLn "Program output:"
       print (eval tpow7_10)
