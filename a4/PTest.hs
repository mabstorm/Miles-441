-- This module checks to make sure the Presburger package is working
-- properly on your assignment.  Once you have established it is
-- working, you can ignore this module

module PTest where
       
import qualified Data.Integer.Presburger as P

form :: P.Formula
form = P.TRUE P.:/\: P.Exists (\x -> x P.:=: x) 

checked :: Bool
checked = P.check form

main = 
  putStrLn ("The result of checking should be True.  It is: " ++ show checked)