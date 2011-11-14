-- COS 441 Homework 3 Part V
-- Our own movie/animation
--
--

import Control.Applicative
import Animation hiding (translate)
import Picture hiding (circle)
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
overP time_off p1 p2  = Beh (\t ->
                         if sin(t*(freq+time_off)) > 0
                             then at (overB p1 p2) t
                             else at (overB p2 p1) t
                       )


at :: Behavior a -> Time -> a
at (Beh f) t = f t


create_cloud :: Float -> Float -> Float -> Behavior Picture

create_cloud cx cy time_off = p1 `overB` p2 `overB` p3 `overB` p4
  where p1 = (translateB (rightx, centery) ell1)
        p2 = (translateB (centerx, topy) ell1)
        p3 = (translateB (centerx, bottomy) ell1)
        p4 = (translateB (leftx, centery) ell1)
        rightx = Beh (\t -> 
                         if sin(t*(freq+time_off)) > 0
                             then (xrad*cos(t*(freq+time_off))+half_xrad+cx)
                             else (-xrad*cos(t*(freq+time_off))+half_xrad+cx))
        leftx = Beh (\t -> 
                         if sin(t*(freq+time_off)) > 0
                             then (xrad*cos(t*(freq+time_off))-half_xrad+cx)
                             else (-xrad*cos(t*(freq+time_off))-half_xrad+cx))
        centerx = Beh (\t -> 
                         if sin(t*(freq+time_off)) > 0
                             then (xrad*cos(t*(freq+time_off))+cx)
                             else (-xrad*cos(t*(freq+time_off))+cx))
        topy = Beh (\t -> cy + half_rad)
        bottomy = Beh (\t -> cy - half_rad)
        centery = Beh (\t -> cy)



poly = lift1 Polygon

kite_size = 0.3

mykite = mypolycyan `overB` mypolyred `overB` kite_line

mypolycyan = reg (lift0 cy) (shape (poly (Beh (\t -> 
  [(kite_size*cos(t),kite_size*sin(t)),
  (kite_size*(cos(t)+1), kite_size*(sin(t))),
  (kite_size*(cos(t)), kite_size*(sin(t)-2)),
  (kite_size*(cos(t)), kite_size*(sin(t)+1)),
  (kite_size*(cos(t)-1), kite_size*(sin(t)))]
  ))))

mypolyred = reg (lift0 Red) (shape (poly (Beh (\t -> 
  [(kite_size*cos(t),kite_size*sin(t)),
  (kite_size*(cos(t)-1), kite_size*(sin(t))),
  (kite_size*(cos(t)), kite_size*(sin(t)-2)),
  (kite_size*(cos(t)), kite_size*(sin(t)+1)),
  (kite_size*(cos(t)+1), kite_size*(sin(t)))]
  ))))

kite_line = reg (lift0 Black) (shape (poly (Beh (\t ->
  [(kite_size*cos(t),kite_size*(sin(t)-2)),
  (kite_size*(cos(t)+0.1),kite_size*(sin(t)-2)),
  (-3,-1.7),
  (-3.1,-1.7)]
  ))))


cw :: Color
cw = White
cb :: Color
cb = Blue
cblue :: Color
cblue = Blue
cg :: Color
cg = Green
cy :: Color
cy = Cyan
cyellow :: Color
cyellow = Yellow

ell_rad :: Behavior Radius
ell_rad = 0.2
half_rad :: Float
half_rad = 0.1
ell_xrad :: Behavior Radius
ell_xrad = 0.5
half_xrad :: Float
half_xrad = 0.4


ground = translateB (Beh (\t->0),Beh (\t->(-6.8))) (reg (lift0 cg) (shape (ell 100 5)))
sky = translateB (Beh (\t->0),Beh (\t->3)) (reg (lift0 cblue) (shape (ell 100 5)))
sun = translateB (Beh (\t->2),Beh (\t->4)) (reg (lift0 cyellow) (shape(ell 2 2)))
obj = translateB (Beh (\t->0),Beh (\t->(-1.5))) (reg (lift0 cy) (shape(ell 0.2 4)))


ell1 = reg (lift0 cw) (shape (ell ell_xrad ell_rad))
to1 = 0.1
to2 = 0.2
to3 = 0.3
to4 = 0.4
to05 = 0.05
to15 = 0.15 

freq :: Float
freq = 0.1
xrad :: Float
xrad = 10

main =
  --do animateB "clouds" (cloud_move ell1 ell2 ell3 ell1b ell2b ell3b 1.9 1.8 2 1 4)
    do animateB "clouds" cloudblock where
     -- cloudblock = cloud1 `overB` cloud2 `overB` cloud3 `overB` cloud4 `overB` cloud5 `overB` cloud6 `overB`  sky `overB` ground where
      cloudblock = mykite `overB` ground `overB` (overP to1 cloud1 cloud2) `overB` (overP to2 cloud3 cloud4) `overB` (overP to05 cloud5 cloud6) `overB` sun `overB` sky `overB` ground where
        --create_cloud cx cy time_off
        cloud1 = create_cloud 4.9 (-0.4) to1
        cloud2 = create_cloud 0.5 0 to15
        cloud3 = create_cloud 0.3 0.9 to2
        cloud4 = create_cloud 2.8 1.2 to3 
        cloud5 = create_cloud 1.5 1.8 to05
        cloud6 = create_cloud 4.3 2.4 to4




