import LambdaFA

-- examples --

-- identity function
x  = "x"
x' = Var x
i  = Abs x x'

-- terminating function
terminator = App (App i i) (App i i)

-- loop
loop = App (Abs x (App x' x')) (Abs x (App x' x'))

-- a function to apply multiple arguments
apps xs =
  aux (reverse xs)
  where
    aux []     = i
    aux [x]    = x
    aux (x:xs) = App (aux xs) x
    
-- encoding booleans --
    
f  = "f"
f' = Var f
t  = "t"
t' = Var t
b  = "b"
b' = Var b
b1 = "b1"
b1' = Var b1
b2 = "b2"
b2' = Var b2

-- \t.\f.t
tru     = Abs t (Abs f t')

-- \t.\f.f
fls     = Abs t (Abs f f')

-- \b.\branch1.\branch2.b branch1 branch2
cond    = Abs b 
          (Abs b1  
           (Abs b2 
            (apps [b', b1', b2'])))