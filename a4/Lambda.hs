-- an implementation of the lambda calculus using higher-order abstract syntax

module Lambda (
  Lam(..),
  freevar,
  eval,
  evals,
  value,
  closed
  ) where

-- HOAS (Higher-Order Abstract Syntax) Lambda Calculus data type follows.  
-- Notice there is no ordinary (bound) variable clause!
-- Only a clause for free variables!
-- Invariant:  all FreeVar Strings should start with "!"
-- To ensure this, only use function freevar to create free variable terms

data Lam =            
    Abs (Lam -> Lam)  -- \x.e
  | App Lam Lam       -- e1 e2
  | FreeVar String    -- a free variable named String  
    
freevar :: String -> Lam
freevar s = FreeVar ("!" ++ s)

-- is a free variable? --
isFreevar (Abs f)     = False
isFreevar (App e1 e2) = False
isFreevar (FreeVar ('!':s)) = True
isFreevar (FreeVar s) = False
    
-- is a value? --
value (Abs f)     = True
value (App e1 e2) = False
value (FreeVar s) = False

-- zero or one step evaluation --
eval :: Lam -> Lam

-- beta rule
eval (App (Abs f) v) | value v = f v

-- app2 rule
eval (App v e2) | value v = 
  let e2' = eval e2 in
  App v e2'

-- app1 rule
eval (App e1 e2) = 
  let e1' = eval e1 in
  App e1' e2

eval (Abs f) = error "Value!"

eval (FreeVar x) = error "Stuck!"

-- multi-step evaluation --
evals :: Lam -> Lam
evals e = if value e then e else evals (eval e)

-- closed --
boundname = "bound"

closed :: Lam -> Bool
closed (Abs f)     = closed (f (FreeVar boundname))
closed (App e1 e2) = closed e1 && closed e2
closed (FreeVar s) = s == boundname

-- printing --

may :: Bool -> String -> String
may b s = if b then s else ""

left, right :: Bool -> String
left b    = may b "("
right b   = may b ")"

-- Bool is true if expression is within an argument position
-- Int is the next variable name to use
showlam :: Bool -> Int -> Lam -> String
showlam b n (FreeVar s) = s 
showlam b n (Abs f) = 
  let var = "x" ++ show n in
  "(" ++ "\\" ++ var ++ "." ++ showlam False (n+1) (f (FreeVar var)) ++ ")"
showlam b n (App e1 e2) =
  left b ++ showlam False n e1 ++ " " ++ showlam True n e2 ++ right b
    
instance Show Lam where
  show x = showlam False 1 x
