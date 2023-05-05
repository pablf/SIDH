-----------------------------------------------------------------------------
-- 
-- Module      :  Field
-- Author      :  pablf
--
-- Stability   : Experimental
--
-- Implementation of elliptic curves (EC), points and arithmetic.
--
-- It also implements j-invariants, check if a point is in an EC, get all points
-- in an Ec and Velu's formulas.
--
-----------------------------------------------------------------------------

module SIDH.EC where

import SIDH.Field

-- | Implementation of Elliptic Curve (EC)
--   always in Weierstrass form: y^2 = x^3 + Ax + B for characteristic != 2, 3.

-- | EC A B
data EC a = EC a a
  deriving Show

-- | Point x y
data Point a = Point a a | O
  deriving (Eq, Show)


-- | To obtain coordinates of points.
getX :: Field a => Point a -> a
getX O = zero
getX (Point x y) = x

getY :: Field a => Point a -> a
getY O = zero
getY (Point x y) = y




-- | Implementation of operations in elliptic curves.
-- | + of points of elliptic curves. We can't make a Num instance because
--   we can't multiply points.
plus :: Field a => EC a -> Point a -> Point a -> Point a
plus _ O p = p
plus _ p O = p
plus ec (Point x1 y1) (Point x2 y2) = case inverse (x2 - x1) of
  Just x  -> Point x3 (l*(x1 - x3) - y1) 
    where  x3 = (l*l - x1 - x2)
           l = (y2 - y1) * x
  Nothing -> if x1 == x2 && y1 == y2 then double ec (Point x1 y1) else O

-- | Double a point, that is, double ec p = plus ec p p, 2p = p + p.
double :: (Field a) => EC a -> Point a -> Point a
double _ O = O
double (EC a b) (Point x y) = case inverse (times 2 y) of
  Just inv -> Point x' (-y)
    where  x' = (l*l - (times 2 x))
           l = ((times 3 x*x) + a) * inv
  Nothing  -> O

-- | Multiply a point p of EC by a number n. It obtains multiples of p:
--      p, 2p, 4p, 8p... and sums them using binary representation.
--   Implemented with toBinary and doublePoints.
mult :: (Field a) => EC a -> Int -> Point a -> Point a
mult ec n p = foldr (\ q p -> plus ec q p) O (doublePoints ec (toBinary n) p)

-- | Transform a Bool list to a list of Points given a point p:
--      in the index i there is:
--                 True  -> 2^(i+1) p
--                 False -> O  (neuter of EC) 
doublePoints :: (Field a) => EC a -> [Bool] -> Point a -> [Point a]
doublePoints _ [] p = []
doublePoints ec (x:xs) p = newP : doublePoints ec xs (double ec p)
  where newP = if x then p else O

-- | Express a number in binary with Bools, e.g., 6 -> [False, True, True]
toBinary :: Int -> [Bool]
toBinary 0 = []
toBinary n = if n `mod` 2 == 0 then False : toBinary (n `quot` 2) else True : toBinary ( (n - 1) `quot` 2)





-- | Auxiliary methods.
-- | j Invariant of an EC. It characterizes an elliptic curve and can be used for cryptographic purposes.
jInvariant :: Field a => EC a -> a
jInvariant (EC a b) = case inverse ((4 `times` a^3) + (27 `times` b^2)) of
  Just inv -> 1728 `times` (4 `times` a^3) * inv
  Nothing -> zero

-- | Check if a point is inside an EC.
pertains :: Field a => EC a -> Point a -> Bool
pertains (EC a b) (Point x y) = y*y == x*x*x + a*x + b

-- | Get all the points in an EC.
--    It is needed that the field a can list all the elements (Reducible a). 
getPoints :: (Field a, Reducible a) => EC a -> [Point a]
getPoints ec = filter (\ x -> x /= O) (getElements >>= \ x -> getSecond ec x)

-- | Get points Point x y in a certain EC where x is fixed.
getSecond :: (Field a, Reducible a) => EC a -> a -> [Point a]
getSecond ec x = map (\ y -> if pertains ec (Point x y) then Point x y else O) getElements







-- | Implementation of Velu formulas.
-- | Kernel represents a subgroup of an EC that will be used as Kernel.
--    It hasn't the point O.
type Kernel a = [Point a]

-- | Obtain the kernel generated by a point p.
getKernel :: Field a => EC a -> Point a -> Kernel a
getKernel ec p = getKernelAcc ec p []

getKernelAcc :: Field a => EC a -> Point a -> Kernel a -> Kernel a
getKernelAcc ec p [] = getKernelAcc ec p [p]
getKernelAcc ec p (q:qs) = let newP = (plus ec p q)
  in if newP == O
     then (q : qs)
     else getKernelAcc ec p (newP : (q : qs))

-- | Compute the function E -> E/K where K is the kernel.
--     (ec, kernel, p) -> q, where q is the point corresponding to p
--   through the morphism ec -> ec/kernel.
veluFunction :: Field a => EC a -> Kernel a -> Point a -> Point a
veluFunction ec kernel (Point x y) = Point x' y'
  where x' = x + sumAll (map (\ p -> getX (plus ec p (Point x y)) - getX p) kernel)
        y' = y + sumAll (map (\ p -> getY (plus ec p (Point x y)) - getY p) kernel)

-- | Obtain the elliptic curve (EC a b)/kernel.
veluEC :: Field a => EC a -> Kernel a -> EC a
veluEC (EC a b) kernel = EC a' b'
  where a' = a - (times 5 (sumAll (map (\ (Point x y) -> (times 3 x*x) + a ) kernel)))
        b' = b - (times 7 (sumAll (map (\ (Point x y) -> (times 5 x*x*x) + (times 3 a*x) + b ) kernel)))





