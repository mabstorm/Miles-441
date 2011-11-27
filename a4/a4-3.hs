import State
import Imp
import Prover
import Hoare

-- variables --
-- for each variable, v :: Exp, v' :: Var

x' = "x"
y' = "y"
a' = "a"
n' = "n"

x   = X x'
y   = X y'
a   = X a'
n   = X n'

-- declarations --

ds = [x', y', a', n']
  
-- programs --

-- all of the triples should be valid
-- whereever you see invalidInv or invalidPre, replace them with your own formula that works
-- all triples you construct should be valid triples that are proven valid using your prover
-- run main function below to determine how many you have constructed correctly
-- when asked to replace a precondition, do not use the trivial precondition "FALSE" 
-- or any formula equivalent to false (ie, don't use (1 :<: 0); don't use (x :=: 1 :&: x :=: 2); etc.)

invalidInv :: BExp
invalidInv = error "Invalid Invariant:  Replace me" 

invalidPre :: BExp
invalidPre = error "Invalid Precondition:  Replace me" 

-- 0 --

prog0 = Prog [] Skip 
trip0 = (invalidPre, prog0, TRUE)     -- Replace invalidPre!

-- 1 --

prog1 = Prog [x'] Skip 
trip1 = (invalidPre, prog1, x :=: 5)  -- Replace invalidPre!

-- 2 --

pre2  = invalidPre                    -- Replace!
c2    = If (a :<: n) Skip (a' := n - 1) 
post2 = a :<: n 
trip2 = (pre2, Prog ds c2, post2)

-- 3 --

pre3  = invalidPre                    -- Replace with something other than FALSE!
inv3  = invalidInv                    -- Replace!
c3 inv =
  While (0 :<: n) (inv) ( 
    n' := n - 1 :> 
    a' := a + 2
    )
prog3 = Prog ds (c3 inv3)
post3 = a :=: 2 * x
trip3 = (pre3, prog3, post3)

-- 4 --

pre4  = TRUE
inv4  = invalidInv                    -- Replace!
c4 inv =
  n' := 9 :>
  a' := 0 :>
  While (0 :<: n) (inv) ( 
    n' := n - 1 :> 
    a' := a + 2
    )
prog4 = Prog ds (c4 inv4)
post4 = a :=: 2 * n
trip4 = (pre4, prog4, post4)

-- 5 --

pre5  = TRUE
inv5  = invalidInv                    -- Replace!
c5 inv =
  n' := 0 :>
  a' := 0 :>
  While (Not (a :=: 7)) (inv) ( 
    a' := a + 2 :>
    n' := n + 1
    )
prog5 = Prog ds (c5 inv5)
post5 = FALSE
trip5 = (pre5, prog5, post5)

-- 6 --

pre6   = invalidPre                  -- Replace with something other than FALSE (or equivalent to FALSE) 
inv6a  = invalidInv                  -- Replace
inv6b  = FALSE
c6 inv1 inv2 =
  While (0 :<: n) (inv1) ( 
    x' := 5 :>
    a' := a + x :>
    n' := n - 1
    )
prog6 = Prog ds (c6 inv6a inv6b)
post6 = a :=: 5 * y
trip6 = (pre6, prog6, post6)

-- 7 --
-- hint:  notice that 7 is similar to 6, but harder ... 

pre7   = invalidPre                  -- Replace (with something not FALSE)
inv7a  = invalidInv                  -- Replace
inv7b  = invalidInv                  -- Replace
c7 inv1 inv2 =
  While (0 :<: n) (inv1) ( 
    x' := 5 :>
    While (0 :<: x) (inv2) ( 
        a' := a + 1 :>
        x' := x - 1
        ) :>
    n' := n - 1
    )
prog7 = Prog ds (c7 inv7a inv7b)
post7 = a :=: 5 * n
trip7 = (pre7, prog7, post7)

-- results --

trips = [trip0, trip1, trip2, trip3, trip4, trip5, trip6, trip7]

results = map (\(pre, prog, post) -> verify pre prog post) trips

main = 
  let rs      = zip [0..] results :: [(Int,Bool)]
      outputs = map (\(n,b) -> putStrLn (show n ++ ": " ++ show b)) rs
  in  sequence_ outputs