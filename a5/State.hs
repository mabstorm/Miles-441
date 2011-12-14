module State where

type Var     = String
type Table a = [(Var, a)]

empty = []

look :: Table a -> Var -> Maybe a
look [] v = Nothing
look ((v',i):xs) v = 
  if v == v' 
  then Just i 
  else look xs v 

up :: Table a -> Var -> a -> Table a
up [] v i = [(v,i)]
up ((v',i'):xs) v i = 
  if v == v' 
  then (v,i):xs 
  else (v',i'):up xs v i
       
initialize :: [Var] -> a -> Table a
initialize [] i = []
initialize (x:xs) i = (x,i) : initialize xs i

dom :: Table a -> [Var]
dom s = map (\(v,i) -> v) s

indom :: Table a -> Var -> Bool 
indom s v = any (\(v',i) -> v == v') s 