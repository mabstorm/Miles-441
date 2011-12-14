COS 441, Assignment 5, Part I

Answer the questions (a), (b), (c), ... (j) embedded below.

Consider the following Monad definition:

> newtype Printer a = P (String, a)
> 
> instance Monad Printer where
>   return v = P ("", v)
>   (>>=) (P (s,v)) f = let P (s',v') = f v in P (s ++ s', v')

This definition (and any proper Monad definition), should obey
the monad laws (for all a, k, m and h with the appropriate types):

(1) (return a) >>= k         =  k a

(2) m >>= return             =  m

(3) m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h

In the following proofs, you may use any of our normal reasoning techniques (ie:
substitution of equals for equals in its various forms) as well as the 
following laws/equations in your proof:  

(Law1) for any Haskell expression e that does not contain the variables s or v,
        e   =   let P (s,v) = e in P (s,v)
                                              
(Law2) for any Haskell expressions m and f of appropriate types,
        m >>= f    =    let P (s,v) = m in (P (s,v) >>= f) 
                                             
(Law3) let P (s,v) = P (e1,e2) in e    =   e[e1/s][e2/v]

(Law4) (s ++ s') ++ s''   =   s ++ (s' ++ s'')

(Law5) s = "" ++ s

(Law6) s = s ++ ""

Law1 is a special case of a law that is usually called "eta-expansion."
Here is a more general form of eta-expansion:

(Eta) for any Haskell pattern pat, and 
      for any Haskell expression e that does not contain variables in pat, 
      e    =   let pat = e in pat
                                             
Law2 is a combination of eta-expansion and this law:

(Permute Let-App)  
  For any pat, f and m, where the variables in pat, f and m are all different,
  f (let pat = m in pat)      =      let pat = m in f pat
                                             
Law3 is an example of our normal law of substitution of equals for equals 
involving patterns.

Questions:

(a)  Do a 2-column proof below that the Printer monad satisfies monad 
equation 1.  

Proof:

...


(b)  Do a 2-column proof below that the Printer monad satisfies monad
equation 2.

Proof:

...


(c) Translate the left-hand side of equation 1 into an equivalent
expression in do notation (ie:  translate "return a >>= k" into 
an equivalent expression in do noation that does not use ">>="):

> lhs1 a k = do
>   error "replace me"


(d) What is the type of lhs1, assuming we are working with the
Printer monad?  (ie: do not give a fully generic type in terms
of some aribitrary monad "m", give the type in terms of the 
specific monad "Printer")

lhs1 :: ????


(e) Translate the left-hand side of equation 3 into an equivalent 
expression written in do notation.  (ie:, translate 
"m >>= (\x -> k x >>= h)" into an equivalent expression in
do notation that does not use ">>=" )

> lhs3 m k h = do
>   error "replace me"


(f) What is the type of lhs3, assuming we are working with the
Printer monad?

lhs3 :: ???


(g) Below, define any new type T that you want and write a *faulty* 
Monad instance definition.  Your instance definition should type
check (write it after ">" so that the literate Haskell type checker
can type check it), but it should fail to obey equation 1.  After writing
your instance definition, give a counter-example and briefly explain 
(a sentence or two) why it fails equation 1.

Monad type and instance definition:

>
>
>

Counter-example and brief explanation of why it fails equation 1:

> k = error "replace me with a counter example"
> a = error "replace me with a counter example"

(h)  Do the same as (g), except this time, write an instance declaration
that fails equations 2.  (You instance declaration could be the
same as in (g) if your instance satisfies neither equation 1 nor 2.)

Monad type and instance definition:

>
>
>

Counter-example and brief explanation of why it fails equation 3:

> m = error "replace me with a counter example"

--------------------------------------------------

Some monads not only satisfy the three laws above but are also
commutative.  In other words, they satisfy the following additional
law (for all m, n, f with the appropriate types):

(4) m >>= (\a -> n >>= (\b -> f a b))
    =
    n >>= (\b -> m >>= (\a -> f a b))

  
(i) Below, write the law above in do notation instead of using >>=. 

do                   
   ...               
   ...                   

= 

do 
   ...
   ...
  

(j) Is the Printer monad defined above commutative?  

Yes or No:  



