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
overP time_off p1 p2  = Beh (\t ->
                         if sin(t*(freq+time_off)) > 0
                             then at (overB p1 p2) t
                             else at (overB p2 p1) t
                       )
at :: Behavior a -> Time -> a
at (Beh f) t = f t


create_cloud :: Float -> Float -> Float -> Behavior Picture

create_cloud cx cy time_off = (overP time_off p1 p1b) `overB` (overP time_off p2 p2b) `overB` (overP time_off p3 p3b) `overB` (overP time_off p4 p4b)
  where p1 = (translateB (rightx, centery) ell1)
        p2 = (translateB (centerx, topy) ell1)
        p3 = (translateB (centerx, bottomy) ell1)
        p4 = (translateB (leftx, centery) ell1)
        p1b = (translateB (rightx, centery) ell1b)
        p2b = (translateB (centerx, topy) ell1b)
        p3b = (translateB (centerx, bottomy) ell1b)
        p4b = (translateB (leftx, centery) ell1b)
--       rightx :: Behavior Float
--       leftx :: Behavior Float
--       centerx :: Behavior Float
--       topy :: Behavior Float
--       bottomy :: Behavior Float
--       centery :: Behavior Float
        rightx = Beh (\t -> xrad*cos(t*(freq+time_off))+half_xrad+cx)
        leftx = Beh (\t -> xrad*cos(t*(freq+time_off))-half_xrad+cx)
        centerx = Beh (\t -> xrad*cos(t*(freq+time_off))+cx)
        topy = Beh (\t -> cy + half_rad)
        bottomy = Beh (\t -> cy - half_rad)
        centery = Beh (\t -> cy)



cw :: Color
cw = White
cb :: Color
cb = Black

ell_rad :: Behavior Radius
ell_rad = 0.2
half_rad :: Float
half_rad = 0.1
ell_xrad :: Behavior Radius
ell_xrad = 0.5
half_xrad :: Float
half_xrad = 0.4



ell1 = reg (lift0 cw) (shape (ell ell_xrad ell_rad))
ell1b = reg (lift0 cb) (shape (ell ell_xrad ell_rad))

freq :: Float
freq = 0.1
xrad :: Float
xrad = 10

main =
  --do animateB "clouds" (cloud_move ell1 ell2 ell3 ell1b ell2b ell3b 1.9 1.8 2 1 4)
    do animateB "clouds" cloudblock where
      cloudblock = cloud1 `overB` cloud2 `overB` cloud3 `overB` cloud4 where
        cloud1 = create_cloud 4.9 1 0.1
        cloud2 = create_cloud 0.3 2 0.1
        cloud3 = create_cloud 1.5 0 0.2
        cloud4 = create_cloud 2.9 1.3 0.3


















--cloud_move :: Behavior Picture -- p1
--     -> Behavior Picture -- p1b
--     -> Behavior Picture -- p2
--     -> Behavior Picture -- p2b
--     -> Behavior Picture -- p3
--     -> Behavior Picture -- p3b
--     -> Behavior Picture -- p3b
--     -> Behavior Picture -- p3b
--     -> Float            -- y1
--     -> Float            -- y2
--     -> Float            -- y3
--     -> Float            -- freq
--     -> Float            -- xrad
--     -> Behavior Picture
--
--cloud_move p1 p2 p3 p4 p1b p2b p3b y1 y2 y3 freq xrad = (overP freq p1' p1b') `overB` (overP freq p2' p2b') `overB` (overP freq p3' p3b')  
--    where p1' = (translateB (x1, by1) p1)
--          p2' = (translateB (floatx, by2) p2)
--          p3' = (translateB (floatx, by3) p3)
--          p1b' = (translateB (x1, by1) p1b)
--          p2b' = (translateB (floatx, by2) p2b)
--          p3b' = (translateB (floatx, by3) p3b)
--          x1 :: Behavior Float
--          x1 = Beh (\t -> xrad*cos(t*freq)+1)
--          floatx :: Behavior Float
--          floatx = Beh (\t -> xrad*cos (t*freq))
--          by1 :: Behavior Float
--          by1 = Beh (\t -> y1)
--          by2 :: Behavior Float
--          by2 = Beh (\t -> y2)
--          by3 :: Behavior Float
--          by3 = Beh (\t -> y3)

