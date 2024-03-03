module Main where

class Symantics repr where
    int :: Int -> repr h Int
    minus :: repr h Int -> repr h Int 

    add :: repr h Int -> repr h Int -> repr h Int
    mult :: repr h Int -> repr h Int -> repr h Int

newtype R h a = R{unR :: h -> a}
eval e = unR e ()

instance Symantics R where
    int x = R $ const x
    minus x = R $ \h -> -(unR x h)

    add e1 e2 = R $ \h -> (unR e1 h) + (unR e2 h)
    mult e1 e2 = R $ \h -> (unR e1 h) * (unR e2 h)


-- Testing

td1 = mult (add (int 1) (int 2)) (minus (int 3))

eval_td1 = eval td1

main = do
       print eval_td1
