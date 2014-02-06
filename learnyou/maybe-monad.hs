import Control.Applicative
import Control.Monad
import Data.Monoid

-- division function that checks for div by 0
mdiv :: Double -> Double -> Maybe Double
mdiv x y = if y /= 0 
           then Just (x / y)
           else Nothing

tests = [
   (Just 10 >>= (mdiv 1)) == Just 0.1,
   (Just 0 >>= (mdiv 1)) == Nothing,
   (Nothing >>= (mdiv 1)) == Nothing
 ]

result = all id tests

type Birds = Int
type Pole = (Birds, Birds)

--landLeft :: Birds -> Pole -> Pole  
--landLeft n (left,right) = (left + n,right)  

--landRight :: Birds -> Pole -> Pole  
--landRight n (left,right) = (left,right + n)  

-- apply argument to a function
x -: f = f x

-- pole = (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2

-- 10 birds on the left side and only 3 on the right?
-- That's sure to send poor Pierre falling through the air! 
-- This is pretty obvious here but what if we had a sequence 
-- of landings like this:
-- pole2 = (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2) 

-- use maybe to check theresult each time:
landleft :: Birds -> Pole -> Maybe Pole
landleft birds (l, r) = if l + birds - 3 > r then Nothing else Just (l + birds, r)

landright :: Birds -> Pole -> Maybe Pole
landright birds (l, r) = if r + birds - 3 > l then Nothing else Just (l, r + birds)

-- a function that always causes the pole to `fail`
banana :: Pole -> Maybe Pole  
banana _ = Nothing  


{--
*Main> Just (0, 0) >>= landleft 1  -- same as return (0, 0) >>= ...
Just (1,0)
*Main> Just (0, 0) >>= landright 1
Just (0,1)
*Main> Just (0, 0) >>= landright 1 >>= landleft 2
Just (2,1)
*Main> Just (0, 0) >>= landright 1 >>= landleft 2 >>= landright 5
Nothing
--}


