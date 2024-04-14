{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Compiler ( compile ) where

import Symantics

import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as Syntax
import GHC.Exts

newtype C a = C ExpQ
unC :: C a -> ExpQ
unC (C x) = x

liftC0 :: Syntax.Lift t => t -> C a
liftC0 x         = C [| x |]

liftC1 :: Q Exp -> C a -> C b
liftC1 g (C a)          = C $ do
                            g' <- g
                            a' <- a
                            return (AppE g' a')

liftC2 :: Q Exp -> C a -> C b -> C c
liftC2 g (C a) (C b) = C $ do
                         g' <- g
                         a' <- a
                         b' <- b
                         return (AppE (AppE g' a') b')

instance Symantics C where
    int = liftC0
    bool = liftC0

    pair (e1, e2) = liftC2 [| (,) |] e1 e2
    first = liftC1 [| fst |]
    second = liftC1 [| snd |]

    minus = liftC1 [| negate |]
    add = liftC2 [| (+) |]
    mult = liftC2 [| (*) |]
    
    not_ = liftC1 [| not |]
    and_ = liftC2 [| (&&) |]
    or_ = liftC2 [| (||) |]

    eq = liftC2 [| (==) |] 
    lt = liftC2 [| (<) |]
    lte = liftC2 [| (<=) |]
    gt = liftC2 [| (>) |]
    gte = liftC2 [| (>=) |]

    if_ c e1 e2 = C $ [| ( if $(unC c) then $(unC e1) else $(unC e2) ) |]

    lam f = C $ do
        name <- newName "x"
        let body = unC (f (C (varE name)))
        lamE [varP name] body
    app = liftC2 [| ($) |]

    fix f = C $ do
        name <- newName "x"
        let body = unC (f (C (varE name)))
        letE [valD (varP name) (normalB body) []] (varE name)

compile :: C a -> ExpQ
compile x = unC x
