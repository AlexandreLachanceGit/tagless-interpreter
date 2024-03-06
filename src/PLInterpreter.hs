-- Interpreter: Usual programming language 

module PLInterpreter ( R(..), eval ) where

import Symantics

newtype R a = R {unR :: a}

instance Symantics R where
    int x = R x
    bool x = R x
    -- pair x = R $ const x

    minus x = R $ -(unR x)
    add e1 e2 = R $ unR e1 + unR e2
    mult e1 e2 = R $ unR e1 * unR e2

    not_ x = R $ not (unR x)
    and_ e1 e2 = R $ unR e1 && unR e2
    or_ e1 e2 = R $ unR e1 || unR e2

    eq e1 e2 = R $ unR e1 == unR e2
    lt e1 e2 = R $ unR e1 < unR e2
    lte e1 e2 = R $ unR e1 <= unR e2
    gt e1 e2 = R $ unR e1 > unR e2
    gte e1 e2 = R $ unR e1 >= unR e2

    if_ c e1 e2 = R $ if unR c then unR e1 else unR e2

    lam f = R $ unR . f . R
    app e1 e2 = R $ unR e1 (unR e2)

    fix f = R $ fx (unR . f . R) where fx f = f (fx f)

eval :: R a -> a
eval = unR
