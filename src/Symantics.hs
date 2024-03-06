module Symantics where

class Symantics repr where
    int :: Int -> repr Int
    bool :: Bool -> repr Bool

    pair :: (repr a, repr b) -> (repr a, repr b)
    first :: (repr a, repr b) -> repr a
    second :: (repr a, repr b) -> repr b

    minus :: repr Int -> repr Int 
    add :: repr Int -> repr Int -> repr Int
    mult :: repr Int -> repr Int -> repr Int

    not_ :: repr Bool -> repr Bool
    and_ :: repr Bool -> repr Bool -> repr Bool
    or_ :: repr Bool -> repr Bool -> repr Bool

    eq :: repr Int -> repr Int -> repr Bool
    lt :: repr Int -> repr Int -> repr Bool
    lte :: repr Int -> repr Int -> repr Bool
    gt :: repr Int -> repr Int -> repr Bool
    gte :: repr Int -> repr Int -> repr Bool

    if_ :: repr Bool -> repr a -> repr a -> repr a

    lam :: (repr a -> repr b) -> repr (a->b)
    app :: repr (a->b) -> repr a -> repr b

    fix :: (repr a -> repr a) -> repr a

