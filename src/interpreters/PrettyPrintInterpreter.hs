-- Interpreter: Computes a "pretty-printed" version of the program

{-# LANGUAGE OverloadedStrings #-}

module PrettyPrintInterpreter (prettyView) where
import Symantics
import qualified Data.Text as T

newtype R a = R {unR :: Int -> T.Text}

instance Symantics R where
    int x = R $ const $ T.pack (show x)
    bool x = R $ const $ T.pack (show x) 

    pair (x, y) = R $ \h -> "(" <> unR x h <> "," <> unR y h <> ")"
    first p = R $ \h -> "(fst " <> unR p h <> ")"
    second p = R $ \h -> "(snd " <> unR p h <> ")"

    minus x = R $ \h -> "-(" <> unR x h <> ")"
    add e1 e2 = R $ \h -> "(" <> unR e1 h <> " + " <> unR e2 h <> ")"
    mult e1 e2 = R $ \h -> "(" <> unR e1 h <> " * " <> unR e2 h <> ")"

    not_ x = R $ \h -> "(not " <> unR x h <> ")"
    and_ e1 e2 = R $ \h -> "(" <> unR e1 h <> " && " <> unR e2 h <> ")"
    or_ e1 e2 = R $ \h -> "(" <> unR e1 h <> " || " <> unR e2 h <> ")"

    eq e1 e2 = R $ \h -> "(" <> unR e1 h <> " == " <> unR e2 h <> ")"
    lt e1 e2 = R $ \h -> "(" <> unR e1 h <> " < " <> unR e2 h <> ")"
    lte e1 e2 = R $ \h -> "(" <> unR e1 h <> " <= " <> unR e2 h <> ")"
    gt e1 e2 = R $ \h -> "(" <> unR e1 h <> " > " <> unR e2 h <> ")"
    gte e1 e2 = R $ \h -> "(" <> unR e1 h <> " >= " <> unR e2 h <> ")"

    if_ c e1 e2 = R $ \h -> "(if " <> unR c h <> " then " <> unR e1 h <> " else " <> unR e2 h <> ")"

    lam f = R $ \h -> let x = "x" <> T.pack (show h)
                      in "(|" <> x <> "| -> " <> unR (f (R $ const x)) (succ h) <> ")"
    app e1 e2 = R $ \h -> "(" <> unR e1 h <> " " <> unR e2 h <> ")"

    fix f = R $ \h -> let self = "self" <> T.pack (show h)
                      in "(fix (|" <> self <> "| -> " <> unR (f (R $ const self)) (succ h) <> "))"

prettyView :: R a -> T.Text
prettyView x = unR x 0

