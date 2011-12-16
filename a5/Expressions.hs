module Expressions where

import State

infix  6 :/:
infixl  5 :++:, :+:

-- Values --

data Value = Str String | Num Int deriving (Eq, Show)

-- Expressions --

data Exp = 
    X Var                 -- a variable
                          -- raise undefined_variable_exception if an
                          --   expression ever refers to a variable that
                          --   has no value in the current state
  | Val Value             -- a value is an expression
    
  -- Expressions with String Arguments --
  
  | Exp :++: Exp          -- concatenate two strings
  | Length Exp            -- Length s returns the integer length of string s
  | PrintThen Exp Exp     -- PrintThen e1 e2 prints string e1 then returns e2
    
  -- Expressions with Integer Arguments --
  
  | Exp :+: Exp           -- add two integers
  | Exp :/: Exp           -- divide two integers; raise an exception if second
                          -- expression is zero
  deriving (Eq, Show)
           
-- Expression Helper Functions --

str :: String -> Exp
str s = Val (Str s)

num :: Int -> Exp
num i = Val (Num i)

-- Exceptions --

newtype Exception = Exception String deriving (Eq, Show)

divide_by_zero_exception       = Exception "divide by zero"
bad_type_exception             = Exception "bad type"
undefined_variable_exception   = Exception "undefined variable"

-- State --

type State = Table Value
  

----------------------------
-- Define your monad here --
----------------------------

--newtype Effects a = Effects a

-- The Effects Monad should be basically the same as Result, but
-- does not use either. Error handling on the way is passed up,
-- and a final part of the main eval function handles whether
-- the passed up thing was a value or an error when returning
-- the Result.

newtype Effects a = Eff (String, a) deriving (Show)

instance Monad Effects where
  return a = Eff ("", a)
  (Eff (a, b)) >>= f = let (Eff (a',b')) = f b in (Eff (a++a',b'))
  fail _ = error "something or other failed"

{-
newtype Effects a = Eff Result

instance Monad Effects where
  return v = Eff (("", Left v))
  Eff ( (Str a, Left b)) >>= f = let (Eff ( (a', b'))) = f b 
     in (Eff (a++a', b'))
  
  Eff ( (Str s, Right e)) >>= f = Eff ( ("error", e))
  fail _ = error "something failed"

newtype Effects a = Eff (String, Value)

instance Monad Effects where
  return a = Eff ("", a)
 -}         
type Result = (String, Either Value Exception)
          
----------------------------------------------------------------------
-- Define any generally useful helper functions for your monad here --
----------------------------------------------------------------------

-- raises exception e in Effects monad
raise :: Exception -> Effects 
raise e = return e

-- additional useful functions for manipulating your monad here ... --

-- these functions return the value assumed by the caller function
-- so an error is raised if the value is incorrect

castToInt :: Value -> State -> Effects String
castToInt (Num i) s = return (show i)
castToInt (Str x) s = raise bad_type_exception


castToString :: Value -> State -> Effects String
castToString (Num i) s = raise bad_type_exception
castToString (Str w) s = return w


eff_str_to_int :: Effects String -> Effects Int
eff_str_to_int x = return (read x :: Int)


-------------------------------
-- Write your evaluator here --
-------------------------------

-- the resulting string is the data that should be printed --
-- also return (Left v) if a normal value v is result of evaluation
--      return (Right e) if exception e is result of evaluation

-- eval is the outer layer of the entire computation, returning the Result

eval :: State -> Exp -> Result
eval s exp = do
  x <- eval s exp
  case x of --use pattern matching to find the errrors
    (Eff (st, (return bad_type_exception))) -> ("error", Right (bad_type_exception))
    (Eff (st, (return divide_by_zero_exception))) -> ("error", Right (divide_by_zero_exception))
    (Eff (st, (return undefined_variable_exception))) -> ("error", Right (undefined_variable_exception))
    (Eff (st, a)) -> (st, Left a)

-- subval takes care of the actual expression computations, resulting in
-- Effects Value, which is similar to Results, but they can also be errors.

subval :: State -> Exp -> Effects Value

subval s (Val (Str st)) = return st

subval s (Val (Num n)) = return n

-- lookup the variable in the context and report if the variable is bad
subval s (X v) =
  let vr = look s v
  in  case vr of
        Nothing -> return undefined_variable_exception
        Just x -> return x

-- + and / check to make sure that the variables are numbers

subval s (a1 :+: a2) = do
  n1 <- subval a1
  n2 <- subval a2
  x <- castToInt n1
  y <- castToInt n2
  if (x==(raise bad_type_exception) || y==(raise bad_type_exception)) then return ("", Right bad_type_exception) else (Eff ("", return (x+y)))

-- raise an exception if trying to divide by zero
subval (a1 :/: a2) = do
  n1 <- subval a1
  n2 <- subval a2
  x <- castToInt n1
  y <- castToInt n2
  if y==0 then raise divide_by_zero_exception else Eff ("", return (x `div` y))

-- print e1 by returning it as the first part of the effect
-- and return e2 after evaluation
subval s (PrintThen e1 e2) = do
  printed <- subval s e1
  returnval <- subval s e2
  printed_string <- castToString printed
  Eff (printed_string, returnval)

subval s (s1 :++: s2) = do
  x <- subval s s1
  y <- subval s s2
  st1 <- castToString x
  st2 <- castToString y
  Eff ("", return (st1 ++ st2))

subval s (Length st) = do
  x <- castToString st
  return (length x)

{-
type Result = (String, Either Value Exception)
eval :: State -> Exp -> Result

--eval s e = ("fail", Right bad_type_exception)

eval s (expr1 :+: expr2) = ("", Left (Num 123))

eval s ((X x1) :+: expr2) = 
    let subval = eval s expr2
        sumval = case subval of
           (str, Left v) -> Num ((num_to_int v) + (num_to_int (Num 1)))
           (str, Right (Exception e)) -> error "implement me"
        stringl = fst (subval)
      in  (stringl, Left sumval) 
eval s ((X x1) :+: (X x2)) =
    ("", Left (Num 5))
eval x (X x1) = ("", Left (Num 3))


eval s e = 
  let somevar = s
      someothervar = e
  in  ("", Left (Num 20000))
-}



-----------------------
-- Some Testing code --
-----------------------

-- this testing code is not complete.  You may want to add more of your own.

-- some variables --
    
-- int variables --
x' = "x"
y' = "y"
z' = "z"

x = X x'
y = X y'
z = X z'

-- string variables --
s' = "s"
t' = "t"
w' = "w"
v' = "v"

s = X s'
t = X t'
w = X w'
v = X v'

-- some states --

st1 :: State
st1 = 
  [
    (x',Num 0),
    (y',Num 1),
    (z',Num 2),
    (s',Str "hi"),
    (t',Str "bi"),
    (w',Str "wow")
    ]
 
-- some expressions --

e0 = PrintThen (str "hello ") (PrintThen (str "world") (num 0))
e1 = x :+: x :+: y :+: num 10
e2 = y :/: x
e3 = PrintThen s (PrintThen t e1)
e4 = PrintThen s (PrintThen t e2)
e5 = PrintThen (PrintThen t e2) e1
e6 = PrintThen (PrintThen t e1) e2
e7 = PrintThen (PrintThen t s) t :++: w
e8 = (x :+: y) :++: s 
e9 = s :++: t :++: w :++: v
e10 = y :+: y :+: y
e11 = y :+: y

es = [e0,e1,e2,e3,e4,e5,e6,e7,e8,e9]

evals :: State -> [Exp] -> [Result]
evals st es = map (eval st) es

tests = evals st1 es

formatTests :: [Result] -> IO ()
formatTests tests = sequence_ (map format ntests)
  where
    ntests :: [(Int,Result)]
    ntests = zip [0..] tests
    
    format :: (Int, Result) -> IO ()
    format (testnum, (str, out)) =
      putStrLn (show testnum ++ ": " ++ formatOut out ++ " | Printing:" ++ str)
      
    formatOut :: Either Value Exception -> String
    formatOut (Left (Num i)) = show i
    formatOut (Left (Str s)) = show s
    formatOut (Right (Exception e)) = "Exception: " ++ e
    
results = formatTests tests


