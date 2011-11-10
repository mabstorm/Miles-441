-- COS 441 Homework 3 Part V
-- Our own movie/animation
--
--

import Control.Applicative
import Animation hiding (translate)
import Picture

translate :: (Float, Float) -> Picture -> Picture
translate v p =
    case p of
      Region c r   -> Region c (Translate v r)
      p1 `Over` p2 -> (translate v p1) `Over` (translate v p2)
      EmptyPic     -> EmptyPic

-- Translate a picture behavior by a given vector behavior
translateB :: (Behavior Float, Behavior Float) -> 
               Behavior Picture -> 
               Behavior Picture
translateB (x,y) p = lift2 translate (zipB (x,y)) p

-- Convert a pair of behaviors into a pair behavior
zipB :: (Behavior a, Behavior b) -> Behavior (a,b)
zipB (Beh b1, Beh b2) = Beh (\t -> (b1 t, b2 t))

-- Construct a circle behavior
circ :: Behavior Float -> Behavior Shape
circ r = ell r r

instance Functor Behavior where
  fmap f (Beh fn_returnsa) = Beh (\t -> f (fn_returnsa t))
instance Applicative Behavior where
  pure f = Beh (\t -> f)
  (Beh f) <*> (Beh x) = Beh (\t -> f t (x t))








