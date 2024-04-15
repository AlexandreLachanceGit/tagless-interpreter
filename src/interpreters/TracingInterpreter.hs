-- A tracing interpreter (i.e. like an interpreter, but it prints out all the steps)

module TracingInterpreter ( trace ) where

import Symantics

type Level = Int
type VarCounter = Int

indent :: Level -> String
indent lvl = replicate (4 * lvl) ' '

format :: Level -> String -> String
format lvl body = (indent lvl ++ body ++ "\n")

newtype T a = T { unT :: Level -> VarCounter -> String }

instance Symantics T where
    int x = T $ \l _ -> format l ("create new int -> " ++ show x)
    bool x = T $ \l _ -> format l ("create new bool -> " ++ show x)

    pair (a, b) = T $ \l c -> 
        let nl = succ l
        in format l ("create pair") ++ unT a nl c ++ unT b nl c 
    first p = T $ \l c ->
        format l ("get first of pair") ++ unT p (succ l) c
    second p = T $ \l c ->
        format l ("get second of pair") ++ unT p (succ l) c

    minus x = T $ \l c -> 
        format l ("minus operation") ++ unT x (succ l) c
    add x y = T $ \l c -> 
        let nl = succ l
        in format l ("add operation") ++ unT x nl c ++ unT y nl c
    mult x y = T $ \l c ->
        let nl = succ l
        in format l ("multiply operation") ++ unT x nl c ++ unT y nl c

    not_ x = T $ \l c -> 
        format l ("not operation") ++ unT x (succ l) c
    and_ x y = T $ \l c ->
        let nl = succ l
        in format l ("and operation") ++ unT x nl c ++ unT y nl c
    or_ x y = T $ \l c ->
        let nl = succ l
        in format l ("or operation") ++ unT x nl c ++ unT y nl c

    eq x y = T $ \l c ->
        let nl = succ l
        in format l ("equal operation") ++ unT x nl c ++ unT y nl c
    lt x y = T $ \l c ->
        let nl = succ l
        in format l ("less than operation") ++ unT x nl c ++ unT y nl c
    lte x y = T $ \l c ->
        let nl = succ l
        in format l ("less than or equal operation") ++ unT x nl c ++ unT y nl c
    gt x y = T $ \l c ->
        let nl = succ l
        in format l ("greater than operation") ++ unT x nl c ++ unT y nl c
    gte x y = T $ \l c ->
        let nl = succ l
        in format l ("greater than or equal operation") ++ unT x nl c ++ unT y nl c

    if_ cond e1 e2 = T $ \l c ->
        let nl = succ l
        in format l ("if statement") ++ unT cond nl c ++ unT e1 nl c ++ unT e2 nl c

    lam f = T $ \l c ->
        let nl = succ l
            x = "x" ++ show c
            af = f (T $ \_ _ -> format (succ nl) x)
        in format l ("lambda abstraction -> " ++ x) ++ unT af nl (succ c)
    app f x = T $ \l c ->
        let nl = succ l
        in format l ("apply function") ++ unT f nl c ++ unT x nl c

    fix f = T $ \l c ->
        let nl = succ l
            self = "self" ++ show c
            af = f (T $ \_ _ -> format (nl) self)
        in format l ("fix function -> " ++ self) ++ unT af nl (succ c)
        
trace :: T a -> String
trace x = unT x 0 0
