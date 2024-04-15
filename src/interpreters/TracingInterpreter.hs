-- A tracing interpreter (i.e. like an interpreter, but it prints out all the steps)

module TracingInterpreter ( eval_trace ) where

import Symantics

type Level = Int
type VarCounter = Int

indent :: Level -> String
indent lvl = replicate (4 * lvl) ' '

format :: Level -> String -> String
format lvl body = (indent lvl ++ body ++ "\n")

newtype T a = T { unT :: Level -> VarCounter -> String }

instance Symantics T where
    -- int x = T $ \lvl _ -> format lvl ("int " ++ show x)
    -- bool x = T $ \lvl _ -> format lvl ("bool " ++ show x)

    -- pair (x, y) = T $ \_ lvl vc -> 
    --     format lvl ("pair\n" ++ (unT x (lvl + 1) vc) ++  (unT y (lvl + 1) vc))
    -- first p = T $ \lvl vc -> format lvl ("first")

eval_trace :: T a -> IO ()
eval_trace x = putStrLn (unT x 0 0)
