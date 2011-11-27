module Hoare (
  wp,
  verify
  ) where

import State
import Imp
import Prover

-- wp f c is the weakest precondition for c with respect to postcondition f

wp :: BExp -> Comm -> BExp
wp f Skip            = f
wp f (x := e)        = error "Write Me!" -- implement the backwards assignment rule using a substitution function
wp f (c1 :> c2)      = let f' = wp f c2 in wp f' c1 
wp f (If b c1 c2)    = let f1 = wp f c1 
                           f2 = wp f c2
                       in (b :==> f1) :&: (Not b :==> f2)
wp f (While b inv c) = inv :&: 
                       (inv :&: b :==> wp inv c) :&: 
                       (inv :&: Not b :==> f)

-- verify p prog q is True when the Hoare Triple { p } c { q } is valid
--   where prog = Prog ds c 
--   verify must check that the precondition p implies wp f c
--   verify must also check that p, c, and q are well-formed with respect to ds
--     if they are well-formed, proceed.
--     if they are not well-formed, use illFormedTriple to signal an error

illFormedTriple = error "Ill-formed Hoare Triple"

verify :: BExp -> Program -> BExp -> Bool
verify p (Prog ds c) q =
  if goodB ds p && good (Prog ds c) && goodB ds q then 
    let f = wp q c in
    prove (p :==> f) ds
  else
    illFormedTriple