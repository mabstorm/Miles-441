import SOE

--------------------------------
--Michael Bailey (mabailey)
--Leon Ho (tlho)
--Assignment 2
--COS 441 Programming Languages
--code.hs
--------------------------------


-------------PART III------------

--Definitions for Unary and Binary data types
data Bin = Nil | O (Bin) | I (Bin) deriving (Show) 
data Un = Nul | S (Un) deriving (Show)


--TO SEMANTIC NUMBERS--

--Converts Unary representation to semantic numbers
unSem Nul = 0
unSem (S a) = 1 + unSem a 

--Converts Binary representation to semantic numbers
binSem a = fst $ binSem2 a


--Converts Binary representation to semantic numbers
binSem2 :: Bin -> (Int, Int)
binSem2 Nil = (0, 0)
binSem2 (I a) = 
  let (cur, i) = binSem2 a in
  (cur + pow2(i), i+1)
binSem2 (O a) = 
  let (cur, i) = binSem2 a in
  (cur, i+1)

--We need to define our own power function for binSem2
pow2 0 = 1
pow2 n = 2 * pow2 (n-1)



--SWITCH BETWEEN REPRESENTATIONS--

--Binary to unary function
bin2un :: Bin -> Un
bin2un Nil = Nul
bin2un a = bin2un3 (Nul, binSem a)

--Subfunctionf or bin2un, recursively builds up the unary representation
bin2un3 :: (Un, Int) -> Un
bin2un3 (a, 0) = a
bin2un3 (a, i) = bin2un3 (S a, i-1)

--Unary to binary function
un2bin :: Un -> Bin
un2bin a = un2bin2 (Nil, unSem a, odd(unSem a))

--Subfunction for un2bin, recursively builds up the binary representation
un2bin2 :: (Bin, Int, Bool) -> Bin
un2bin2 (a, 0, bo) = a
un2bin2 (a, i, bo) =
	case bo of
	 True -> un2bin2(I a, (i-1) `div` 2, odd((i-1) `div` 2))
	 False -> un2bin2(O a, i `div` 2, odd(i `div` 2))


--------------PART IV-----------------

------a-------
--Function for returning tuples of the form a -> (a,a+1) from a list
pairAndOneRec :: [Integer] -> [(Integer,Integer)]
pairAndOneRec [ ] = [ ]
pairAndOneRec (x:[ ]) = pairAndOneRec2 x
pairAndOneRec (x:xs) = (pairAndOneRec2 x) ++ (pairAndOneRec xs) 

--Subfunction for actually grouping the paired value with the input value
pairAndOneRec2 :: Integer -> [(Integer, Integer)]
pairAndOneRec2 x = [(x, x+1)]


------b--------
--Function as above but using map
pairAndOneNon :: [Integer] -> [(Integer,Integer)]
pairAndOneNon a = map pairAndOneNon2 a
--Mapped function
pairAndOneNon2 :: Integer -> (Integer,Integer)
pairAndOneNon2 a = (a,a+1)


------c-------
--Function that returns [[in,in+1,..][in,in+1,...]...] = [SUMin,SUMin+1,...]
addPWRec :: [[Integer]] -> [Integer]
addPWRec [ ] = [ ]
addPWRec (x:xs) = helpPWRec(x,addPWRec(xs))

helpPWRec :: ([Integer],[Integer]) -> [Integer]
helpPWRec (x:xs, y:ys) = [x+y]++helpPWRec(xs,ys)
helpPWRec (x:xs, []) = [x]++helpPWRec(xs,[])
helpPWRec ([], y:ys) = [y]++helpPWRec([],ys)
helpPWRec ([], []) = []


-----d-------
--Using non-recursive for the above, uses foldl and maps
addPWNon :: [[Integer]] -> [Integer]
addPWNon a =
   let (x:xs) = padding ([[]] ++ a) in
   foldl (\sums currentlist -> zipWith (+) sums currentlist) x xs

padding :: [[Integer]] -> [[Integer]]
padding a = 
    let m = maximum $ map length a in 
    map (\t -> t ++ (take (m - (length t)) $ repeat 0)) a

-----e-------
--Function that returns the minimum positive element in a list
minListRec :: [Integer] -> Integer
minListRec (x) = minimum (if (cleanMinList x)==[] then [0] else (cleanMinList x))

cleanMinList :: [Integer] -> [Integer]
cleanMinList [] = []
cleanMinList (x:xs) = 
	if x > (-1) then
	 x : cleanMinList(xs)
	else
	 cleanMinList(xs)


-----f-------
--use filter() to get rid of the non-positive elements, return lowest remaining
minListNon :: [Integer] -> Integer
minListNon x = minimum(if (filter (> (-1)) x)==[] then [0] else filter (> (-1)) x)




----------PART V--------------
-----a-------
--Sierpinski's Carpet, call 'carpet' function
--adapted from sierpinski.hs from the 441 website

--draws a square
fillSq :: Window -> Int -> Int -> Int -> IO ()
fillSq w x y size =
  drawInWindow w (withColor Blue 
                    (polygon [(x,y), (x + size, y), (x, y - size), (x+size, y-size)]))

minSize :: Int
minSize = 8

--main recursive part
square :: Window -> Int -> Int -> Int -> IO ()
square w x y size =
  if size <= minSize then 
    fillSq w x y size 
  else 
    let size2 = size `div` 3 in
    do square w x y size2
       square w (x + size2) y size2
       square w (x + (2*size2)) y size2
       square w x (y - size2) size2
       square w x (y - (2*size2)) size2
       square w (x + (2*size2)) (y-size2) size2
       square w (x + size2) (y-(2*size2)) size2
       square w (x + (2*size2)) (y-(2*size2)) size2

--hit the spacebar to close
spaceClose :: Window -> IO ()
spaceClose w =
  do k <- getKey w
     if k == ' ' then closeWindow w
 		 else spaceClose w

--main function
carpet =
  runGraphics $ do
    w <- openWindow "Sierpinski's Carpet" (360, 360)
    square w 50 300 256
    spaceClose w



-----b------
--Own fractal, 3D view over patch of colorful fruits, looking off into the distance
--Call 'fractal' function to use

xfactor :: Int
xfactor = 2
yfactor_t :: Int
yfactor_t = 5
yfactor_b :: Int
yfactor_b = 8

--not really circles, but rather our base image of "fruits" and "grass"
fillCircle :: Window -> Int -> Int -> Int -> Color -> IO ()
fillCircle w x y size color =
  do drawInWindow w (withColor Green
		    (polygon [(x,y), (x, y+(((yfactor_t * size) `div` (yfactor_b`div`2)))), ((x + (size*xfactor), y+(((yfactor_t* size) `div` (yfactor_b`div`2))))), (x+size,y+( ((yfactor_t * size) `div` yfactor_b))), (x + (size*xfactor), y)]))
     drawInWindow w (withColor Green
		    (polygon [(x,y), (x,y-(size*xfactor)),((x-((yfactor_t*size)`div`yfactor_b)),y-size),((x-((yfactor_t*size)`div`(yfactor_b`div`2))),y-(size*xfactor)),(x-((yfactor_t*size)`div`(yfactor_b`div`2)),y)]))
     drawInWindow w (withColor color
		    (ellipse (x-size,y-size) (x+size,y+size)))


minSize2 :: Int
minSize2 = 3

--main recursive part
circle :: Window -> Int -> Int -> Int -> Color -> IO ()
circle w x y size color = 
  if size <= minSize2 then
    fillCircle w x y size color
  else 
    let size2 = size `div` 3 in
    do fillCircle w x y size color
       circle w (x + (size*xfactor)) (y+((yfactor_t* size) `div` yfactor_b)) ((yfactor_t*size) `div` yfactor_b) Cyan
       circle w (x-((yfactor_t*size)`div`yfactor_b)) (y-(size*xfactor)) ((yfactor_t*size)`div`yfactor_b) Magenta
       
--main function for fractal
fractal =
  runGraphics $ do
    w <- openWindow "Sierpinski's Carpet" (1000, 1000)
    circle w 0 500 300 Cyan
    spaceClose w





