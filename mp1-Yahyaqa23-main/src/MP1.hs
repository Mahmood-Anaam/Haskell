module MP1 where


import Graphics.Gloss
import Data.List

-- Part 1: Polymorphic functions from types

p1_1 :: a -> b -> b
p1_1 a b = b


p1_2 :: (a -> b -> c) -> (a,b) -> c
p1_2 f (x,y)= f x y 


p1_3 :: (a -> b) -> (b -> c) -> a -> c
p1_3 f g x=g (f x)


p1_4 :: (a -> b -> c) -> a -> (d -> b) -> d -> c
p1_4 f x g y= f x (g y)


-- Part 2: Function implementations 


-- 1. Transposes a 2-row x 2-column tuple.
--
--    e.g., transposeTup ((1,2),(3,4)) = ((1,3),(2,4))
transposeTup :: ((a,b),(c,d))  -- input matrix
             -> ((a,c),(b,d))  -- transposed matrix
transposeTup ((a,b),(c,d))= ((a,c),(b,d))


-- 2. Sorts the elements of a 3-tuple.
--
--    e.g., sort3Tup (2,1,3) = (1,2,3)
--          sort3Tup (3,2,1) = (1,2,3)
sort3Tup :: Ord a 
         => (a,a,a)  -- input 3-tuple
         -> (a,a,a)  -- sorted 3-tuple

sort3Tup (a,b,c)= (mi,me,mx) where 
                  s=sort([a,b,c])
                  mx=last s
                  mi=head s
                  me=head(tail(s))


-- 3. Computes the compound interest earned.
--    e.g., compoundInterest 100 0.2 1 = 20
--          compoundInterest 100 0.2 2 = 44
compoundInterest :: Floating a 
                 => a   -- principal
                 -> a   -- rate
                 -> Int -- num of compounding periods
                 -> a   -- amount of compound interest earned
compoundInterest p r 0=0                 
compoundInterest p r n=p*r+ compoundInterest (p*r+p) r (n-1)


-- 4. Computes the length of the Collatz sequence starting at the input.
--
--    e.g., collatzLen 1 = 0
--          collatzLen 6 = 8
--          collatzLen 27 = 111
collatzLen :: Integer  -- start value of the sequence
           -> Integer  -- length of sequence

collatzLen 1=0
collatzLen n=if n `rem` 2==0  then 1+collatzLen (n`div` 2)  else  1+collatzLen ((3*n)+1)


-- 5. Computes the square root of the input using Newton's method.
--
--    e.g., newtonsSqrt 2 ~= 1.4142...
--          newtonsSqrt 1000 ~= 31.6227...
newtonsSqrt :: (Floating a, Ord a) 
            => a -- x
            -> a -- square root of x

newtonsSqrt x= improve x 1

goodEnough x r e= (abs(r*r-x)) < e

improve x r  =if goodEnough x r 0.001
             then r else improve x ((r+(x/r))/2)






-- 6. Draws a planet in a circular orbit given an orbital radius and period.
drawOrbit :: Float  -- radius
          -> Float  -- period
          -> Float  -- time
          -> Picture
drawOrbit _ _ _ = blank


-- 7. Draws a planet in an elliptical orbit based on Kepler's equation.
drawOrbit' :: Float  -- semi-major axis
           -> Float  -- eccentricity
           -> Float  -- period
           -> Float  -- time
           -> Picture
drawOrbit' _ _ _ _ = blank


meanMotion :: Floating a 
           => a -- period
           -> a -- mean motion
meanMotion p = undefined


meanAnomaly :: Floating a 
            => a -- mean motion
            -> a -- time
            -> a -- mean anomaly
meanAnomaly n t = undefined


eccentricAnomaly :: (Floating a, Ord a) 
                 => a -- eccentricity
                 -> a -- mean anomaly
                 -> a -- eccentric anomaly
eccentricAnomaly ecc ma = undefined


trueAnomaly :: (Floating a, Ord a) 
            => a -- eccentricity
            -> a -- eccentric anomaly
            -> a -- true anomaly
trueAnomaly ecc ea = undefined


heliocentricDist :: Floating a 
                 => a -- semi-major axis
                 -> a -- eccentricity
                 -> a -- eccentric anomaly
                 -> a -- heliocentric distance
heliocentricDist a ecc ea = undefined
