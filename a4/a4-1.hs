import Lambda as H
import LambdaFA as F
import Test.QuickCheck

-- We have done a qualified import so we can distinguish between
-- the two kinds of lambda forms.  The higher-order lambda expressions
-- from the Lambda module need to be prefixed with H. and first-order lambda
-- expressions with F. 

-- higher-order lambda terms:
zeroH, oneH, twoH :: H.Lam
zeroH = H.Abs (\s -> H.Abs (\z -> z))
oneH  = H.Abs (\s -> H.Abs (\z -> H.App s z))
twoH  = H.Abs (\s -> H.Abs (\z -> H.App s (H.App s z)))
threeH  = H.Abs (\s -> H.Abs (\z -> H.App s (H.App s (H.App s z))))
badH  = H.Abs (\s -> H.Abs (\z -> H.App s (H.App z s)))

-- an equivalent version of zeroH:
-- this is a stupid way to compute the higher-order term zero,
-- but it is correct.  The idea I'm illustrating is the fact
-- that you sometimes may want to embed a (somewhat complicated)
-- function like zero inside an H.Abs where that function computes
-- the body of lambda abstraction that you need when an argument is applied
-- To see how this works, run ghci. Then enter both zeroH and zeroH' at the
-- prompt.  You will see that they both print out exactly the same thing.
zeroH' = H.Abs (\s -> H.Abs zero)
  where
    zero :: H.Lam -> H.Lam
    zero body = aux 7 body
    aux n body = if n == 0 then body else aux (n-1) body 

-- first-order lambda terms:
zeroF, oneF, twoF :: F.Lam
zeroF = F.Abs "s" (F.Abs "z" (F.Var "z"))
oneF  = F.Abs "s" (F.Abs "z" (F.App (F.Var "s") (F.Var "z")))
twoF  = F.Abs "s" (F.Abs "z" (F.App (F.Var "s") (F.App (F.Var "s") (F.Var "z"))))

badF  = F.Abs "s" (F.Abs "z" (F.App (F.Var "s") (F.App (F.Var "z") (F.Var "s"))))

-- convert Int into F.Lam lambda calculus term
-- may raise an exception if n < 0
intToLamF :: Int -> F.Lam
intToLamF n = if n < 0 then error "n must be non-negative" else F.Abs "s" (F.Abs "z" (inc n))
  where
    inc :: Int -> F.Lam
    inc n = aux n
    aux n = if n == 0 then (F.Var "z") else F.App (F.Var "s") (aux (n-1))

-- convert Int into H.Lam lambda calculus term
-- may raise an exception if n < 0
-- Note: when you create a Haskell function to help represent a Lambda term, that Haskell function
-- may have a lot of Haskell code inside it that when executed generates the appropriate Lambda term
intToLamH :: Int -> H.Lam
intToLamH n = if n < 0 then error "n must be non-negative" else H.Abs (\s -> H.Abs (inc s))
  where
    inc :: H.Lam -> H.Lam -> H.Lam
    inc body s = aux n body s
    aux n body s = if n == 0 then s else H.App body (aux (n-1) body s) 

-- convert an F.Lam representing a number into an integer
-- assume that argument l does not contain any free variables.
-- raise an exception using error if the argument l does not have the form \s.\z.s (s (... z))
-- your code should work no matter what string name someone has used for "s" and for "z"
-- ie: both F.Abs "s" (F.Abs "z" (F.Var "z")) and F.Abs "foo" (F.Abs "bar" (F.Var "bar"))
--     are valid representations of zero
lamFToInt :: F.Lam -> Int
lamFToInt l = getInt l
  where
    getInt l = aux 0 l "" ""
    aux n (F.Abs s lam) firstval secondval = 
      if (firstval)==""
        then aux (n) lam s ""
        else if (secondval==""&&firstval/=secondval)
          then aux (n) lam firstval s
          else error "not well-formed argument l code:3"

    aux n (F.App (F.Var fvar) lam2) firstval secondval = 
      if (firstval=="" || secondval=="" || fvar/=firstval)
        then error "not well-formed argument l code:4"
        else aux (n+1) lam2 firstval secondval
    aux n (F.Var fvar) firstval secondval =
      if fvar/=secondval
        then error "not well-formed argument l code:5"
        else n
    aux n lam firstval secondval = error "not well-formed argument l code:6"
           

--threeH  = H.Abs (\s -> H.Abs (\z -> H.App s (H.App s (H.App s z))))

-- convert an H.Lam representing a number into an integer
-- assume that argument l does not contain any free variables.
-- raise an exception using error if the argument l does not have the form \s.\z.s (s (... z))
{-
lamHToInt :: H.Lam -> Int
lamHToInt l = getInt l
  where
    getInt l = aux 0 l "" ""
    aux n (H.Abs (\ a -> (lam))) firstval secondval = 
      if firstval==""
        then aux n lam (showlam False 0 s) ""
        else if (secondval==""&&firstval/=secondval)
          then aux n lam firstval (showlam False 0 s)
          else error "not well-formed argument l code:3"

    aux n (H.App fvar lam2) firstval secondval = 
      if (firstval=="" || secondval=="" || (showlam False 0 fvar)/=firstval)
        then error "not well-formed argument l code:4"
        else aux (n+1) lam2 firstval secondval
    aux n fvar firstval secondval =
      if (showlam False 0 fvar)/=secondval
        then error "not well-formed argument l code:5"
        else n
-}
-- Testing --
-- these tests are not necessarily complete --
--myCheck p = quickCheckWith args p
--  where
--    args = Args {replay = Nothing, maxSuccess = 10, maxDiscard = 20, maxSize = 50, chatty = True}

--check :: (Int -> a) -> (a -> Int) -> IO ()
--check intToLam lamToInt = myCheck f
--  where
--    f :: Int -> Property
--    f n = (n >= 0) ==> (n == (lamToInt . intToLam) n) -- if n >= 0 then do the conversion
    
-- run the definitions below in GHCI to do some preliminary tests to see if your functions are working
--checkF = check intToLamF lamFToInt
--checkH = check intToLamH lamHToInt

