COS 441 Homework 3


> import Control.Applicative
> import Animation hiding (planets, translate)
> import Picture

Part IV

This part of the homework involves creating a model solar system.

> translate :: (Float, Float) -> Picture -> Picture
> translate v p =
>     case p of
>       Region c r   -> Region c (Translate v r)
>       p1 `Over` p2 -> (translate v p1) `Over` (translate v p2)
>       EmptyPic     -> EmptyPic
>
> -- Translate a picture behavior by a given vector behavior
> translateB :: (Behavior Float, Behavior Float) -> 
>                Behavior Picture -> 
>                Behavior Picture
> translateB (x,y) p = lift2 translate (zipB (x,y)) p
>
> -- Convert a pair of behaviors into a pair behavior
> zipB :: (Behavior a, Behavior b) -> Behavior (a,b)
> zipB (Beh b1, Beh b2) = Beh (\t -> (b1 t, b2 t))
>
> -- Construct a circle behavior
> circ :: Behavior Float -> Behavior Shape
> circ r = ell r r
>
> sun :: Behavior Picture
> sun = reg (lift0 Yellow) (shape (circ 1))


> planets :: Behavior Picture
> planets = sun
>
> main :: IO()
> main = 
>   do animateB "Solar system" sun 

(orbit sun mercury 2.0 2.0 0.2)
       
It may be useful for you to use your applicative functor for Behaviors.  Feel
free to copy it in here and use it:

> instance Functor Behavior where
>   fmap f (Beh fn_returnsa) = Beh (\t -> f (fn_returnsa t))
> instance Applicative Behavior where
>   pure f = Beh (\t -> f)
>   (Beh f) <*> (Beh x) = Beh (\t -> f t (x t))

Next, use the provided function translateB to write a function
       
> orbit :: Behavior Picture -- the satellite
>       -> Behavior Picture -- the fixed body
>       -> Float            -- the frequency of the orbit
>       -> Float            -- the x-radius of the orbit
>       -> Float            -- the y-radius of the orbit
>       -> Behavior Picture
> orbit_depth :: Behavior Picture -- the satellite
>       -> Behavior Picture -- the fixed body
>       -> Float            -- the frequency of the orbit
>       -> Float            -- the x-radius of the orbit
>       -> Float            -- the y-radius of the orbit
>       -> Behavior Picture
> orbit_size :: Behavior Picture -- the satellite
>       -> Float            -- the frequency of the orbit
>       -> Float            -- the x-radius of the orbit
>       -> Float            -- the y-radius of the orbit
>       -> Behavior Picture
>
> overB = lift2 Over
> overB_depth_size freq p1 p2 = Beh (\t ->
>                          if sin(t*freq) < 0
>                              then at (lift2 Over p1 p2) t
>                              else at (lift2 Over p2 p1) t
>                        )
> at :: Behavior a -> Time -> a
> at (Beh f) t = f t


> orbit p1 p2 freq xrad yrad = overB p1 translated_p
>      where translated_p = (translateB (floatx, floaty) p2)
>            floatx :: Behavior Float
>            floatx = Beh (\t -> xrad*cos (t*freq))
>            floaty :: Behavior Float
>            floaty = Beh (\t -> yrad*sin (t*freq))

> orbit_depth p1 p2 freq xrad yrad = overB_depth_size freq p1 translated_p
>      where translated_p = (translateB (floatx, floaty) p2)
>            floatx :: Behavior Float
>            floatx = Beh (\t -> xrad*cos (t*freq))
>            floaty :: Behavior Float
>            floaty = Beh (\t -> yrad*sin (t*freq))

> orbit_size p1 freq xrad yrad = overB_depth_size freq p1 translated_p
>      where moving_p = reg (lift0 Red) (shape (circ size1))
>            size1 = Beh (\t -> sin(freq*t/2.0))
>            translated_p = (translateB (floatx, floaty) moving_p)
>            floatx :: Behavior Float
>            floatx = Beh (\t -> xrad*cos (t*freq))
>            floaty :: Behavior Float
>            floaty = Beh (\t -> yrad*sin (t*freq))
>
> mercury :: Behavior Picture
> mercury = reg (lift0 Red) (shape (circ 0.1))
> sun2 :: Behavior Picture
> sun2 = reg (lift0 Yellow) (shape (circ 2.0))


> -- running this definition in ghci should create the appropriate animation
> orbitTest = do animateB "Solar system" (orbit sun mercury 2.0 2.0 0.2)
> -- running this definition in ghci should create the next animation
> orbitTest' = do animateB "Solar system" (orbit_depth sun mercury 2.0 2.0 0.2)
> -- running this definition in ghci should create the next animation
> orbitTest'' = do animateB "Solar system" (orbit_size sun2 2.0 3.1 0.2)


