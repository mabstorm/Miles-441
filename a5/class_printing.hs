

newtype Output a = Out (String, a) 

instance Monad Output where
 return v = Out ("", v)
 (Out (s,v)) >>= f = let (s',v') = f v in (s ++ s', v')


