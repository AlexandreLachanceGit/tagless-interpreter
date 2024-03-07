{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Symantics
import PLInterpreter
import LengthInterpreter

-- Testing

td1 = mult (add (int 1) (int 2)) (minus (int 3))

td2 = and_ (bool True) (bool False)
td3 = not_ (or_ (bool True) (bool False))

td4 = and_ (gte (int 2) (add (int 1) (int 1))) (lt (minus (int 1)) (int 1))

td5 = if_ (eq (int 2) (int 2)) (int 1) (int 2)

td6 = second (pair (int 14, bool False))


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


eval_td1 = eval td1
eval_td2 = eval td2
eval_td3 = eval td3
eval_td4 = eval td4
eval_td5 = eval td5
eval_td6 = eval td6
eval_td7 = eval tpow72

main :: IO ()
main = do
       print eval_td1
       print eval_td2
       print eval_td3
       print eval_td4
       print eval_td5
       print eval_td6
       print eval_td7
       print (len td1)
       print (len (app (lam (\x -> (add x (int 1)))) (int 1)))
       print (len tpow72)
