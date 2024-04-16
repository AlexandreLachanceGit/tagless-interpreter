-- Partial Evaluator

module PartialEval ( partial_eval ) where

import Symantics
import Control.Monad

extract :: Partial repr a -> repr a
extract (P a _) = a

inject :: repr a -> Partial repr a 
inject a = P a Nothing

data Partial repr a = P { dynamic :: repr a, static :: Maybe a }

instance Symantics repr => Symantics (Partial repr) where
    int x = P (int x) (Just x)
    bool x = P (bool x) (Just x)

    pair (P e1 a, P e2 b) = inject (pair (e1, e2)) 
    first (P e _) = inject (first e)
    second (P e _) = inject (second e)

    minus (P e x) = 
        case x of
            Just 0 -> P e x
            Just x -> int (-x)
            _ -> inject (minus (e))
    add (P e1 a) (P e2 b) = 
        case (a, b) of
            (Just 0, _) -> P e2 b
            (_, Just 0) -> P e1 a
            (Just x, Just y) -> int (x + y)
            _ -> inject (add (e1) (e2)) 
    mult (P e1 a) (P e2 b) = 
        case (a, b) of
            (Just 0, _) -> P e1 a
            (_, Just 0) -> P e2 b
            (Just x, Just y) -> int (x * y)
            _ -> inject (mult (e1) (e2)) 

    not_ (P e a) = 
        case a of
            Just b -> bool (not b)
            _ -> inject (not_ e)
    and_ (P e1 a) (P e2 b) = 
        case (a, b) of
            (Just x, Just y) -> bool (x && y)
            _ -> inject (and_ e1 e2)
    or_ (P e1 a) (P e2 b) = 
        case (a, b) of
            (Just x, Just y) -> bool (x || y)
            _ -> inject (or_ e1 e2)

    eq (P e1 a) (P e2 b) = 
        case (a, b) of
            (Just x, Just y) -> bool (x == y)
            _ -> inject (eq e1 e2)
    lt (P e1 a) (P e2 b) = 
        case (a, b) of
            (Just x, Just y) -> bool (x < y)
            _ -> inject (eq e1 e2)
    lte (P e1 a) (P e2 b) = 
        case (a, b) of
            (Just x, Just y) -> bool (x <= y)
            _ -> inject (eq e1 e2)
    gt (P e1 a) (P e2 b) = 
        case (a, b) of
            (Just x, Just y) -> bool (x > y)
            _ -> inject (eq e1 e2)
    gte (P e1 a) (P e2 b) = 
        case (a, b) of
            (Just x, Just y) -> bool (x >= y)
            _ -> inject (eq e1 e2)

    if_ (P cond a) (P e1 b) (P e2 c) = 
        case a of
            Just True -> P e1 b
            Just False -> P e2 c
            _ -> inject (if_ cond e1 e2)

    
    -- limitation of symantics, 
    -- changing symantics would break all other interpreters
    lam f = inject (lam (extract . f . inject))
    app (P e1 a) (P e2 b) = case a of
        Just f -> P (app e1 e2) (f <$> b)
        _ -> inject (app e1 e2)

    fix f = inject (fix (extract . f . inject))


partial_eval :: Partial repr a -> repr a
partial_eval = extract

