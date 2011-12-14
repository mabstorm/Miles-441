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

newtype Effects a = Int -- redefine this type!

instance Monad Effects where
  return v = error "implement me"
  
  m >>= f  = error "implement me"

                  
----------------------------------------------------------------------
-- Define any generally useful helper functions for your monad here --
----------------------------------------------------------------------

-- raises exception e in Effects monad
raise :: Exception -> Effects a
raise e = error "implement me"

-- additional useful functions for manipulating your monad here ... --

-------------------------------
-- Write your evaluator here --
-------------------------------

-- the resulting string is the data that should be printed --
-- also return (Left v) if a normal value v is result of evaluation
--      return (Right e) if exception e is result of evaluation

type Result = (String, Either Value Exception)
eval :: State -> Exp -> Result
eval s e = error "implement me"
      
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