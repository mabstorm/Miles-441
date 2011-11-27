import Lambda

-- examples --

-- identity function
i = Abs (\x -> x)

-- terminating function
terminator = App (App i i) (App i i)

-- loop
loop = App (Abs (\x -> App x x)) (Abs (\x -> App x x))

-- a lambda function with a free variable
-- evaluation will get "stuck"

badExp = App i (freevar "free")

-- a function to apply multiple arguments
apps xs =
  aux (reverse xs)
  where
    aux []     = i
    aux [x]    = x
    aux (x:xs) = App (aux xs) x
    
-- encoding booleans --

tru     = Abs (\t -> Abs (\f -> t))
fls     = Abs (\t -> Abs (\f -> f))
cond    = Abs (\b -> 
                Abs (\branch1 -> 
                      Abs (\branch2 -> 
                            apps [b, branch1, branch2])))

bprog0  = apps [tru, fls, fls]
bprog1  = apps [cond, tru, i, tru]

conjoin = Abs (\b1 -> 
                Abs (\b2 ->  
                      apps [b1, b2, fls])) -- "and" is taken in prelude

bprog2  = apps [cond, apps [conjoin, tru, fls], i, tru]