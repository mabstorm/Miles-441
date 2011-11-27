module Imp where

import State

infix  4 :=:, :<:
infixl 3 :&:
infixl 2 :|:
infixl 1 :==>
infix  1 :=
infixl 0 :>

data Exp = 
    X Var 
  | Num Int
  | Add Exp Exp
  | Sub Exp Exp
  | Mult Int Exp 
  deriving (Eq, Show)
    
instance Num Exp where
  e1 + e2       = Add e1 e2
  e1 * e2       = case e1 of 
                    Num i -> Mult i e2  
                    _     -> error "Exp multiplication i * e where i is a constant"
  e1 - e2       = Sub e1 e2
  negate e      = Sub (Num 0) e
  abs e         = error "abs not defined for Exp"
  signum        = error "signum not defined for Exp"
  fromInteger i = Num ((fromInteger i)::Int)
  
data BExp =
    TRUE
  | FALSE
  | Exp  :=: Exp
  | Exp  :<: Exp
  | BExp :&: BExp
  | BExp :|: BExp
  | BExp :==> BExp
  | Not BExp  
  deriving (Eq, Show)
    
data Comm =
    Skip
  | Var := Exp
  | Comm :> Comm          -- sequencing
  | If BExp Comm Comm
  | While BExp BExp Comm  -- the first BExp is the loop condition
                          -- the second BExp is the loop invariant
                          -- the Comm is the loop body
  deriving (Eq, Show)
           
type Declarations = [Var]
data Program = Prog Declarations Comm

-- Well-formedness --

contains :: (Eq a) => [a] -> a -> Bool
contains xs v = any (\x -> x == v) xs

good :: Program -> Bool
good (Prog ds c) = error "Write Me!"

-- should sort for efficiency.
goodState :: State -> Declarations -> Bool
goodState s ds = error "Write Me!"

-- evaluation --

evalE :: State -> Exp -> Int
evalE s (X v)       = look s v 
evalE s (Num i)     = i
evalE s (Add e1 e2) = evalE s e1 + evalE s e2
evalE s (Sub e1 e2) = evalE s e1 - evalE s e2
evalE s (Mult i e)  = i * evalE s e 

evalB :: State -> BExp -> Bool 
evalB s TRUE         = True
evalB s FALSE        = False
evalB s (e1 :=: e2)  = evalE s e1 == evalE s e2
evalB s (e1 :<: e2)  = evalE s e1 <  evalE s e2
evalB s (b1 :&: b2)  = evalB s b1 && evalB s b2
evalB s (b1 :|: b2)  = evalB s b1 || evalB s b2
evalB s (b1 :==> b2) = not (evalB s b1) || evalB s b2
evalB s (Not b)      = not $ evalB s b

evalC :: State -> Comm -> State
evalC s Skip            = s
evalC s (x := e)        = up s x (evalE s e)
evalC s (c1 :> c2)      = let s' = evalC s c1 in evalC s' c2
evalC s (If b c1 c2)    = if evalB s b then evalC s c1 else evalC s c2
evalC s (While b inv c) = while s b c  
                            where while s b c = 
                                    if evalB s b then while (evalC s c) b c  
                                    else s
                                    
eval :: State -> Program -> State
eval s (Prog ds c) = 
  if good (Prog ds c) && goodState s ds then evalC s c
  else error "evaluating ill-formed program"