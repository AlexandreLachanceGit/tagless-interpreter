{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

class Symantics repr where
    int :: Int -> repr h Int
    bool :: Bool -> repr h Bool

    minus :: repr h Int -> repr h Int 
    add :: repr h Int -> repr h Int -> repr h Int
    mult :: repr h Int -> repr h Int -> repr h Int

    not_ :: repr h Bool -> repr h Bool
    and_ :: repr h Bool -> repr h Bool -> repr h Bool
    or_ :: repr h Bool -> repr h Bool -> repr h Bool

    eq :: repr h Int -> repr h Int -> repr h Bool
    lt :: repr h Int -> repr h Int -> repr h Bool
    lte :: repr h Int -> repr h Int -> repr h Bool
    gt :: repr h Int -> repr h Int -> repr h Bool
    gte :: repr h Int -> repr h Int -> repr h Bool

    if_ :: repr h Bool -> repr h t -> repr h t -> repr h t


-- Interpreter: Usual programming language 

newtype R h a = R{unR :: h -> a}

instance Symantics R where
    int x = R $ const x
    bool x = R $ const x

    minus x = R $ \h -> -(unR x h)
    add e1 e2 = R $ \h -> unR e1 h + unR e2 h
    mult e1 e2 = R $ \h -> unR e1 h * unR e2 h

    not_ x = R $ \h -> not (unR x h)
    and_ e1 e2 = R $ \h -> unR e1 h && unR e2 h
    or_ e1 e2 = R $ \h -> unR e1 h || unR e2 h

    eq e1 e2 = R $ \h -> unR e1 h == unR e2 h
    lt e1 e2 = R $ \h -> unR e1 h < unR e2 h
    lte e1 e2 = R $ \h -> unR e1 h <= unR e2 h
    gt e1 e2 = R $ \h -> unR e1 h > unR e2 h
    gte e1 e2 = R $ \h -> unR e1 h >= unR e2 h

    if_ c e1 e2 = R $ \h -> if unR c h then unR e1 h else unR e2 h

eval e = unR e ()

-- Testing

td1 = mult (add (int 1) (int 2)) (minus (int 3))

td2 = and_ (bool True) (bool False)
td3 = not_ (or_ (bool True) (bool False))

td4 = and_ (gte (int 2) (add (int 1) (int 1))) (lt (minus (int 1)) (int 1))

td5 = if_ (eq (int 2) (int 2)) (int 1) (int 2)

eval_td1 = eval td1
eval_td2 = eval td2
eval_td3 = eval td3
eval_td4 = eval td4
eval_td5 = eval td5

main = do
       print eval_td1
       print eval_td2
       print eval_td3
       print eval_td4
       print eval_td5
