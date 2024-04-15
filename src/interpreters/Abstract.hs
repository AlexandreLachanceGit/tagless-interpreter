-- Abstract Interpreter

module Abstract where

-- module Abstract ( abstract ) where
--
-- import SymanticsB
--
-- data State = On | Off | AnyState deriving (Eq)
-- data Sign = Positive | NonPositive | Zero | Negative | NonNegative | AnySign deriving (Eq)
--
-- type family Semantics2 a :: *
-- type instance Semantics2 Bool_          = State
-- type instance Semantics2 Int_           = Sign
-- type instance Semantics2 (a :-> b)      = Semantics2 a -> Semantics2 b
-- type instance Semantics2 (Pair_ a b)    = (Semantics2 a, Semantics2 b)
-- type instance Semantics2 (Either_ a b)  = Either (Semantics2 a) (Semantics2 b)
--
-- newtype A a = A (Semantics2 a)
-- type instance Semantics2 (A a) = A a
--
-- unA :: A a -> Semantics2 a
-- unA (A x) = x
--
-- instance SymanticsB A where
--     -- TODO
--
-- abstract :: A a -> Semantics2 a
-- abstract = unA
