-- Testing

module Play where

import Test.QuickCheck
import State
import Imp
import Prover
import Hoare
import qualified Data.Integer.Presburger as P

-- defining some identifiers and states --

x' = "x"
y' = "y"
a' = "a"
n' = "n"

foo' = "foo"
bar' = "bar"
baz' = "baz"
bof' = "bof"

g1 = [x', y', a', n']
ds = g1
g2 = [foo', bar', baz', bof']
g3 = [x', y', a', n', foo', bar', baz', bof']

x   = X x'
y   = X y'
a   = X a'
n   = X n'
foo = X foo'
bar = X bar'
baz = X baz'
bof = X bof'

s1 = initialize g1 0
s2 = initialize g2 0
s3 = initialize g3 0

-- Testing boolean evaluation --

b1 = foo :=: foo :&: bar :=: bar  :&: TRUE :&: Not FALSE
b2 = foo :=: foo :&: Not (bar :=: bar)
b3 = foo :=: foo :|: Not (bar :=: bar)

check0 = evalB s2 b2 -- False

check1 = all (\b -> evalB s2 b) [b1,b3] -- True

-- Testing command evaluation --

const = Num 7

p1 =
 a' := Num 7 :>
 a' := Add a (Num 3)
 
check2 = look (evalC s1 p1) a' -- 10

p2 =
 a' := 7 :>
 a' := a + 3
 
check3 = look (evalC s1 p2) a' -- 10

p3 =
  a' := 9 :>
  If (a :<: n) 
    (x' := 1) 
    (x' := -1)

check4 = evalC s1 p3
  
p4' inv =
  n' := 9 :>
  a' := 0 :>
  While (0 :<: n) (inv) ( 
    n' := n - 1 :> 
    a' := a + 2
    )
  
p4badinv = FALSE 

p4 = p4' p4badinv

p4post = a :=: 2 * n

check5 = evalC s1 p4

-- Testing well-formedness --

wf1 = goodC g1 p4
wf2 = goodC g2 p4

p5 =
  n' := 9 :>
  a' := 0 :>
  While (0 :<: n) (TRUE) ( -- inv is True
    n' := n - 1 :> 
    foo' := foo + 100
    )
  
wf3 = goodC g1 p5

-- Testing WP

f1 = wp TRUE p1
f2 = wp (a :=: 10) p1
f3 = wp p4post p4

-- theorem proving --

pr1 = P.TRUE
pr2 = P.Exists (\ x -> x P.:=: x)

pf1 = prove b1 g2 -- True
pf2 = prove b2 g2 -- False
pf3 = prove b3 g2 -- True

pf1' = prove f1 g2 -- True
pf2' = prove f2 g2 -- True
pf3' = prove f3 g2 -- True

-- Hoare Triples

prog1 = Prog [] Skip
ht1 = verify TRUE prog1 TRUE
ht2 = verify FALSE prog1 TRUE
ht3 = verify FALSE prog1 FALSE
ht4 = verify TRUE prog1 FALSE

prog2 = Prog g1 p4
ht5 = verify TRUE prog2 p4post

prog3 = Prog g1 p4bad
ht6 = verify TRUE prog3 p4post

