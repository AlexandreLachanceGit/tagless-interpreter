{-# LANGUAGE TypeOperators, TypeFamilies, ConstraintKinds #-}

-- Abstract Interpreter

module Abstract ( abstract ) where

import Symantics

data State = On | Off | AnyState deriving (Eq)
data Sign = Positive | NonPositive | Zero | Negative | NonNegative | AnySign deriving (Eq)

type family Semantics2 a :: * 
-- type instance Semantics2 Bool_          = State
-- type instance Semantics2 Int_           = Sign
-- type instance Semantics2 (a :-> b)      = Semantics2 a -> Semantics2 b
-- type instance Semantics2 (Pair_ a b)    = (Semantics2 a, Semantics2 b)
-- type instance Semantics2 (Either_ a b)  = Either (Semantics2 a) (Semantics2 b)

newtype A a = A (Semantics2 a)
type instance Semantics2 (A a) = A a

unA :: A a -> Semantics2 a
unA (A x) = x

instance Symantics A where
    -- int x = A $ if x > 0 then 
    --         Positive
    --     else if x < 0 then 
    --         Negative
    --     else 
    --         Zero
    --
    -- bool x = A $ if x then
    --         On
    --     else
    --         Off
    --
    -- pair p = A $ Pair_ (fst p) (snd p)
    -- first p = A $ fst p
    -- second p = A $ snd p
    --
    -- minus x = A $ case (unA x) of
    --     Positive -> Negative
    --     Negative -> Positive
    --     Zero -> Zero
    --     _ -> AnySign
    -- add x y = A $ case (unA x, unA y) of
    --     (_, Zero) -> x
    --     (Zero, _) -> y
    --     (Positive, Positive) -> Positive
    --     (Negative, Negative) -> Negative
    --     _ -> AnySign
    -- mult x y = A $ case (unA x, unA y) of
    --     (_, Zero) -> Zero
    --     (Zero, _) -> Zero
    --     (Positive, Positive) -> Positive
    --     (Negative, Negative) -> Positive
    --     (Positive, Negative) -> Negative
    --     (Negative, Positive) -> Negative
    --     _ -> AnySign
    --
    -- not_ x = A $ case (unA x) of
    --     On -> Off
    --     Off -> On
    --     _ -> AnyState
    -- and_ x y = A $ case (unA x, unA y) of
    --     (Off, _) -> Off
    --     (_, Off) -> Off
    --     (On, On) -> On
    --     _ -> AnyState
    -- or_ x y = A $ case (unA x, unA y) of
    --     (On, _) -> On
    --     (_, On) -> On
    --     (Off, Off) -> Off
    --     _ -> AnyState
    --
    -- eq x y = A $ if x == y then
    --             On
    --         else
    --             AnyState
    -- lt x y = A $ case (unA x, unA y) of
    --     (Negative, Positive) -> On
    --     (Negative, Zero) -> On
    --     (Positive, Negative) -> Off
    --     (Positive, Zero) -> Off
    --     _ -> AnyState
    -- lte x y = A $ case (unA x, unA y) of
    --     (Negative, Positive) -> On
    --     (Negative, Zero) -> On
    --     (Positive, Negative) -> Off
    --     (Positive, Zero) -> Off
    --     _ -> AnyState
    -- gt x y = A $ case (unA x, unA y) of
    --     (Positive, Negative) -> On
    --     (Zero, Negative) -> On
    --     (Negative, Positive) -> Off
    --     (Zero, Positive) -> Off
    --     _ -> AnyState
    -- gte x y = A $ case (unA x, unA y) of
    --     (Positive, Negative) -> On
    --     (Zero, Negative) -> On
    --     (Negative, Positive) -> Off
    --     (Zero, Positive) -> Off
    --     _ -> AnyState
    --
    -- if_ c x y = A $ case (unA c, unA x, unA y) of
    --     (On, _, _) -> x
    --     (Off, _, _) -> y
    --     _ -> Either_ x y
    --
    -- -- To get more out of the following, we would need to modify Symantics
    -- -- modifying Symantics would break all the other interpreters
    -- lam f = A $ unA . f . A
    -- app x y = A $ unA x (unA y)
    --
    -- fix f = A $ unA . f . A

abstract :: A a -> Semantics2 a
abstract = unA
