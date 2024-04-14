-- Interpreter: Computes a "pretty-printed" version of the program

{-# LANGUAGE OverloadedStrings #-}

module PrettyPrintInterpreter (prettyView) where
import Symantics
import qualified Data.Text as T

type Level = Int
type VarCounter = Int

indent :: Level -> T.Text
indent n = T.replicate (4 * n) (T.singleton ' ')

newtype R a = R {unR :: Level -> VarCounter -> T.Text}

instance Symantics R where
    int x = R $ const $ const $ T.pack (show x)
    bool x = R $ const $ const $ T.pack (show x) 

    pair (x, y) = R $ \lvl vc -> "(" <> unR x lvl vc <> "," <> unR y lvl vc <> ")"
    first p = R $ \lvl vc -> "(first " <> unR p lvl vc <> ")"
    second p = R $ \lvl vc -> "(second " <> unR p lvl vc <> ")"

    minus x = R $ \lvl vc -> "-(" <> unR x lvl vc <> ")"
    add e1 e2 = R $ \lvl vc -> "(" <> unR e1 lvl vc <> " + " <> unR e2 lvl vc <> ")"
    mult e1 e2 = R $ \lvl vc -> "(" <> unR e1 lvl vc <> " * " <> unR e2 lvl vc <> ")"

    not_ x = R $ \lvl vc -> "(not " <> unR x lvl vc <> ")"
    and_ e1 e2 = R $ \lvl vc -> "(" <> unR e1 lvl vc <> " && " <> unR e2 lvl vc <> ")"
    or_ e1 e2 = R $ \lvl vc -> "(" <> unR e1 lvl vc <> " || " <> unR e2 lvl vc <> ")"

    eq e1 e2 = R $ \lvl vc -> "(" <> unR e1 lvl vc <> " == " <> unR e2 lvl vc <> ")"
    lt e1 e2 = R $ \lvl vc -> "(" <> unR e1 lvl vc <> " < " <> unR e2 lvl vc <> ")"
    lte e1 e2 = R $ \lvl vc -> "(" <> unR e1 lvl vc <> " <= " <> unR e2 lvl vc <> ")"
    gt e1 e2 = R $ \lvl vc -> "(" <> unR e1 lvl vc <> " > " <> unR e2 lvl vc <> ")"
    gte e1 e2 = R $ \lvl vc -> "(" <> unR e1 lvl vc <> " >= " <> unR e2 lvl vc <> ")"

    if_ c e1 e2 = R $ \lvl vc -> indent lvl <> "if " <> unR c lvl vc <> " then\n" <>
        indent (lvl + 1) <> unR e1 (lvl + 1) vc <>
        "\n" <> indent lvl <> "else\n" <>
        indent (lvl + 1) <> unR e2 (lvl + 1) vc 

    lam f = R $ \lvl vc -> let x = "x" <> T.pack (show vc)
                      in indent lvl <> "|" <> x <> "| -> \n" <> unR (f (R $ const $ const x)) (lvl + 1) (succ vc)
    app e1 e2 = R $ \lvl vc -> "(" <> unR e1 lvl vc <> ") (" <> unR e2 lvl vc <> ")"

    fix f = R $ \lvl vc -> let self = "self" <> T.pack (show vc)
                      in indent lvl <> "fix (|" <> self <> "| -> \n" <> unR (f (R $ const $ const self)) (lvl + 1) (succ vc) <> ")"

prettyView :: R a -> T.Text
prettyView x = unR x 0 0

