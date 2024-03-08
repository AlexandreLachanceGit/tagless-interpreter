-- Interpreter: Computes length of program 

module LengthInterpreter ( len ) where

import Symantics

newtype R a = R {unR :: Int}

instance Symantics R where
    int x = R 1
    bool x = R 1

    pair (x, y) = R $ unR x + unR y + 1
    first p = R $ unR p + 1
    second p = R $ unR p + 1

    minus x = R $ unR x + 1
    add e1 e2 = R $ unR e1 + unR e2 + 1
    mult e1 e2 = R $ unR e1 + unR e2 + 1

    not_ x = R $ unR x + 1
    and_ e1 e2 = R $ unR e1 + unR e2 + 1
    or_ e1 e2 = R $ unR e1 + unR e2 + 1

    eq e1 e2 = R $ unR e1 + unR e2 + 1
    lt e1 e2 = R $ unR e1 + unR e2 + 1
    lte e1 e2 = R $ unR e1 + unR e2 + 1
    gt e1 e2 = R $ unR e1 + unR e2 + 1
    gte e1 e2 = R $ unR e1 + unR e2 + 1

    if_ c e1 e2 = R $ unR c + unR e1 + unR e2 + 1

    lam f = R $ (unR . f . R) 0 + 1
    app e1 e2 = R $ unR e1 + unR e2 + 1

    fix f = R $ (unR . f . R) 0 + 1

len :: R a -> Int
len = unR

