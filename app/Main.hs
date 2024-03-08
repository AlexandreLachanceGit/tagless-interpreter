{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import qualified Data.Text.IO as TIO

import Symantics
import PLInterpreter
import HaskellRepInterpreter
import LengthInterpreter
import PrettyPrintInterpreter

-- Testing

td1 = mult (add (int 1) (int 2)) (minus (int 3))

td2 = and_ (bool True) (bool False)
td3 = not_ (or_ (bool True) (bool False))

td4 = and_ (gte (int 2) (add (int 1) (int 1))) (lt (minus (int 1)) (int 1))

td5 = if_ (eq (int 2) (int 2)) (int 1) (int 2)

td6 = second (pair (add (int 14) (int 2), bool False))


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

tpow7 = lam (\x -> app (app tpow x) (int 7))
tpow72 = app tpow7 (int 10)

-- main :: IO ()
main = do
       print (eval td1)
       print (eval td2)
       print (eval td3)
       print (eval td4)
       print (eval td5)
       print (eval td6)
       print (eval (first (pair (add (int 1) (int 3), int 2))))
       print (len td1)
       print (len (app (lam (\x -> (add x (int 1)))) (int 1)))
       print (len tpow72)
       print (len td6)
       TIO.putStrLn (haskellView td6)
       TIO.putStrLn (haskellView td4)
       TIO.putStrLn (haskellView tpow72)
       TIO.putStrLn (haskellView tpow)
       TIO.putStrLn (prettyView td6)
       TIO.putStrLn (prettyView td4)
       TIO.putStrLn (prettyView tpow72)
       TIO.putStrLn (prettyView tpow)
