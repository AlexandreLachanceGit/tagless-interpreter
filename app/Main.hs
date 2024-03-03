module Main where

class Symantics repr where
    int :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int

    lam :: (repr a -> repr b) -> repr (a -> b)
    app :: repr (a -> b) -> repr a -> repr b

instance Symantics R where
    int x = R x
    add e1 e2 = R $ unR e1 + unR e2

    lam f = R $ unR . f . R
    app e1 e2 = R $ (unR e1) (unR e2)

main :: IO ()
main = putStrLn "Hello, Haskell!"
