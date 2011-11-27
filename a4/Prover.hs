module Prover (
  prove
  ) where

import State
import Imp
import qualified Data.Integer.Presburger as P

-- Your primary job is to implement the function prove which
-- proves a BExp is Valid given a list of its free variables 
-- This function assumes the list of free variables given is a
-- super-set of (or equal to) the actual set of free variables
-- The main job of this routine is to convert the boolean expression
-- (it has type BExp) in to a value with type P.Formula and use the
-- check function supplied by the presburger arithmetic package

-- We have supplied you with some helper routines (or the beginnings
-- of some helper routines) get you started.

-- convert an ordinary integer into an equivalent P.Term
int2term :: Int -> P.Term
int2term i = fromInteger (toInteger i)

-- convert a BExp b into a P.Formula given a list xs of its free variables
convert :: BExp -> [Var] -> P.Formula
convert b xs = error "implement me"

-- prove b is valid (return True) or not (return False) given a list xs of its free variables
-- if b contains free variables not in xs, you may raise an error exception
prove :: BExp -> [Var] -> Bool
prove b xs = let f = convert b xs in P.check f