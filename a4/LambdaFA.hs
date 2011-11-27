-- an implementation of the lambda calculus using higher-order abstract syntax

module LambdaFA (
  Lam (Var,Abs,App),
  eval,
  evals,
  value,
  closed
  ) where

-- a typical data structure for implementing lambda expressions 
-- that we won't be using because we are lazy and don't want to
-- implement substitution:

data Lam =
    Var String         -- variables
  | Abs String Lam     -- \"x". e
  | App Lam Lam        -- e1 e2 
    
-- substitution for closed values v --
-- subst e x v == e[v/x]

subst (Var y) x v     = if x == y then v else Var y
subst (Abs y e) x v   = if x == y then (Abs y e) else (Abs y (subst e x v))
subst (App e1 e2) x v = App (subst e1 x v) (subst e2 x v) 
    
-- is a value? --
value (Var s)     = False
value (Abs x e)   = True
value (App e1 e2) = False

-- one step evaluation --
eval :: Lam -> Lam

-- beta rule
eval (App (Abs x e) v) | value v = subst e x v           

-- app2 rule 
eval (App v e2)        | value v = 
  let e2' = eval e2 in  
  App v e2'

-- app1 rule
eval (App e1 e2) = 
  let e1' = eval e1 in  
  App e1' e2
                                   
-- no rule exists
eval (Abs x e) = error "Value!"        
eval (Var x)   = error "Stuck!"

-- multi-step evaluation --
evals :: Lam -> Lam
evals e = if value e then e else evals (eval e)

-- closed --

closed :: Lam -> Bool
closed e = clos [] e 
           where
             clos env (Abs x e)   = clos (x:env) e 
             clos env (App e1 e2) = clos env e1 && clos env e2
             clos env (Var x)     = lookup env x 
             
             lookup [] x      = False
             lookup (y:env) x = x == y || lookup env x 
             
-- printing --

may :: Bool -> String -> String
may b s = if b then s else ""

left, right :: Bool -> String
left b    = may b "("
right b   = may b ")"

-- Bool is true if expression is within an argument position
-- Int is the next variable name to use
showlam :: Bool -> Lam -> String
showlam b (Var s)     = s 
showlam b (Abs x e)   = "(" ++ "\\" ++ x ++ "." ++ showlam False e ++ ")"
showlam b (App e1 e2) =
  left b ++ showlam False e1 ++ " " ++ showlam True e2 ++ right b
    
instance Show Lam where
  show x = showlam False x
