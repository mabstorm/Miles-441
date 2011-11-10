-- COS 441 Homework 3 Part V
-- Our own movie/animation
--
--

import Control.Applicative
import Animation hiding (translate)
import Picture
import Robot hiding (main,spaceClose,at)
import SOE

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


-- a = runGraphics $
--      do w <- openWindowEx "Robot World" (Just (0,0)) 
--                (Just (xWin,yWin)) drawBufferedGraphic
--         drawLine w Green (2,200) (100,200)
--         spaceClose w

overB = lift2 Over
overP freq p1 p2 = Beh (\t ->
                         if sin(t*freq) > 0
                             then at (overB p1 p2) t
                             else at (overB p2 p1) t
                       )
at :: Behavior a -> Time -> a
at (Beh f) t = f t

ground_move :: Behavior Picture -- p1
      -> Behavior Picture -- p1b
      -> Behavior Picture -- p2
      -> Behavior Picture -- p2b
      -> Behavior Picture -- p3
      -> Behavior Picture -- p3b
      -> Float            -- y1
      -> Float            -- y2
      -> Float            -- y3
      -> Float            -- freq
      -> Float            -- xrad
      -> Behavior Picture

ground_move p1 p2 p3 p1b p2b p3b y1 y2 y3 freq xrad = (overP freq p1' p1b') `overB` (overP freq p2' p2b') `overB` (overP freq p3' p3b')  
     where p1' = (translateB (x1, by1) p1)
           p2' = (translateB (floatx, by2) p2)
           p3' = (translateB (floatx, by3) p3)
           p1b' = (translateB (x1, by1) p1b)
           p2b' = (translateB (floatx, by2) p2b)
           p3b' = (translateB (floatx, by3) p3b)
           x1 :: Behavior Float
           x1 = Beh (\t -> xrad*cos(t*freq)+1)
           floatx :: Behavior Float
           floatx = Beh (\t -> xrad*cos (t*freq))
           by1 :: Behavior Float
           by1 = Beh (\t -> y1)
           by2 :: Behavior Float
           by2 = Beh (\t -> y2)
           by3 :: Behavior Float
           by3 = Beh (\t -> y3)

cw :: Color
cw = White
cb :: Color
cb = Black

ell1 = reg (lift0 cw) (shape (ell 1 0.1))
ell2 = reg (lift0 cw) (shape (ell 1 0.1))
ell3 = reg (lift0 cw) (shape (ell 1 0.1))
ell1b = reg (lift0 cb) (shape (ell 1 0.1))
ell2b = reg (lift0 cb) (shape (ell 1 0.1))
ell3b = reg (lift0 cb) (shape (ell 1 0.1))

main =
  do animateB "Ground" (ground_move ell1 ell2 ell3 ell1b ell2b ell3b 0 (-2.0) 2 1 4)




