-----------------------------------------------------------------------------
-- 
-- Module      :  Field
-- Author      :  pablf
--
-- Stability   : Experimental
--
-- General definition of field and construction of types for finite fields.
-- Checking that the characteristic is prime would be difficult.
-- Maybe it could be done using a preconstructed list with a bound for primes.
--
-- Implementation of Field, Module, finite rings F_n for n :: Nat, of polynomial ring with
-- coefficients in F_n and Galois Fields.
--
-- Some aliases for fields are defined.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}

module SIDH.Field where

import GHC.TypeLits
import Data.Proxy

import SIDH.NatList

--
-- | Definition of basic classes: Module and Field.
--   Class Reducible has two methods.
--        - reduce gives an Int associated to the element a, if it is possible
--     in a meaningful way,
--        - getElements gives a list with all the elements of type a, if possible.
class (Show a, Eq a, Num a) => Module a where
  zero :: a
  times :: Int -> a -> a
  
class (Eq a, Num a) => Reducible a where
  reduce :: a -> Int
  getElements :: [a]
  
class Module a => Field a where
  unit :: a
  inverse :: a -> Maybe a

  
sumAll :: Field a => [a] -> a
sumAll [] = zero
sumAll (x:xs) = x + sumAll xs

  

-- | Implementation of rings F_n, for n Nat
--
--    Fn m represent a value m (mod n) but n is specified using Nat kind.
data Fn (n :: Nat) = Fn Int
  deriving Show
  

-- | Define operations to make Fn n a Field
--
--   Use KnownNat to extract value n from type (n :: Nat)

-- | Eq
instance KnownNat n => Eq (Fn n) where
  Fn m == Fn m' = (m - m') `mod` (fromIntegral n) == 0
    where n = natVal (Proxy :: Proxy n)

-- | Num
instance KnownNat n => Num (Fn n) where
  Fn m + Fn m' = Fn ((m + m') `mod` n)
    where n = fromIntegral (natVal (Proxy :: Proxy n))
  Fn m * Fn m' = Fn ((m * m') `mod` n)
    where n = fromIntegral (natVal (Proxy :: Proxy n))
  Fn m - Fn m' = Fn ((m - m') `mod` n)
    where n = fromIntegral (natVal (Proxy :: Proxy n))
  negate (Fn m) = Fn ((- m) `mod` n)
    where n = fromIntegral (natVal (Proxy :: Proxy n))
    
  abs (Fn m) = Fn m
  signum (Fn m) = Fn 1
  fromInteger m = Fn (fromInteger m)
  
-- | Module
instance KnownNat n => Module (Fn n) where
  zero = Fn 0
  times k x | k == 0 = zero
            | k < 0  = - (times (-k) x)
            | k > 0  = x + (times (k - 1) x)
                 
-- | Reducible
instance KnownNat n => Reducible (Fn n) where
  reduce (Fn m) = m `mod` fromIntegral (natVal (Proxy :: Proxy n))
  getElements = map (\ x -> Fn x) [0..(n-1)]
    where n = fromIntegral (natVal (Proxy :: Proxy n))
    
-- | Field
instance KnownNat n => Field (Fn n) where
  unit = Fn 1
  inverse (Fn m) = case n of
                        0         -> Just (Fn 0)
                        1         -> if m == 0 then Nothing else Just (Fn 1)
                        2         -> if m == 0 then Nothing else Just (Fn m)
                        otherwise -> if m == 0 then Nothing else Just (Fn (m^(n-2)))
                     where n = natVal (Proxy :: Proxy n)
  
                     
                     
-- | Implementation of polynomials with coefficients in F_n.
--   Representation using a list, e.g., 2X+3X^2 = [0,2,3]
data Pol (n :: Nat) = Pol [Fn n]
  deriving (Show, Eq)

-- | Num, define ring operations for polynomials.
instance KnownNat n => Num (Pol n) where
  Pol p + Pol q   = simplify $ Pol (map (\ (x,y) -> x + y) (customZip p q zero))
  
  Pol [] * Pol q = Pol []
  Pol p * Pol [] = Pol []
  Pol p * Pol [x] = Pol (map (\ y -> x * y) p)
  Pol [x] * Pol p = Pol (map (\ y -> x * y) p)
  Pol (x:p) * Pol q = timesX 1 (Pol p * Pol q) + (Pol  [x] * Pol q)
  
  negate (Pol p) = Pol (map negate p)
  
  abs (Pol p) = Pol (map abs p)
  signum (Pol p) = Pol []
  fromInteger m = Pol [Fn (fromInteger m)]
  
-- | Module
instance KnownNat n => Module (Pol n) where
  zero = Pol []
  times k (Pol p) = Pol (map (\ x -> times k x) p)

timesF :: KnownNat n => Fn n -> Pol n -> Pol n
timesF (Fn k) (Pol p) = Pol (map (\ x -> times k x) p)
  
  
  
-- | Auxiliary methods for Pol type:
-- | Simplify polynomial
simplify :: KnownNat n => Pol n -> Pol n
simplify (Pol p) = let (Pol q) = simplifyAux (Pol (reverse p)) in Pol (reverse q)

simplifyAux :: KnownNat n => Pol n -> Pol n
simplifyAux (Pol [])     = Pol []
simplifyAux (Pol (x:xs)) = if zero == x then simplifyAux (Pol xs) else Pol (x:xs)

-- | Multiply polynomial p times X, repeat this k times.
timesX :: KnownNat n => Int -> Pol n -> Pol n
timesX 0 p = p
timesX k (Pol p) = timesX (k-1) (Pol (zero : p))

-- | Zip of lists that continues when a list ends using an element elem.
customZip :: [a] -> [a] -> a -> [(a, a)]
customZip [] [] _ = []
customZip [] (y:ys) elem = (elem, y) : customZip [] ys elem
customZip (x:xs) [] elem = (x, elem) : customZip xs [] elem
customZip (x:xs) (y:ys) elem = (x, y) : customZip xs ys elem

-- | Get the coefficients mod n of a polynomial in a list
extract :: KnownNat n => Pol n -> [Nat]
extract (Pol q) = map (\ x -> toNat (reduce x)) q

-- | Remainder of polynomial division: remainder p q is the remainder of p divided by q.
remainder :: KnownNat n => Pol n -> Pol n -> Pol n
remainder (Pol p) (Pol q) = let d = deg (Pol p) - deg (Pol q) in 
  if d < 0 then (Pol p) else case inverseLast (Pol q) of
                         Just inv -> remainder ((Pol p) - (timesF (last p) (Pol q) * (timesX d (Pol [inv])))) (Pol q)
                         Nothing  -> remainder (Pol p) (Pol (init q))
  
-- | Division: division p q is (quotient, remainder) of p divided by q.
division :: KnownNat n => Pol n -> Pol n -> (Pol n, Pol n)
division (Pol p) (Pol q) = let d = deg (Pol p) - deg (Pol q) in 
  if d < 0 then (zero, Pol p) else case inverseLast (Pol q) of
                         Just inv -> let (a, b) = division ((Pol p) - (timesF (last p) (Pol q) * (timesX d (Pol [inv])))) (Pol q)
                           in (a + timesF (last p) (timesX d (Pol [inv])), b)
                         Nothing  -> division (Pol p) (Pol (init q))

-- | Extended Euclidean Algorithm
data EEA a = EEA a a a

startEea :: KnownNat n => Pol n -> Pol n -> EEA (Pol n)
startEea p q = eea (EEA p (Pol [unit]) zero) (EEA q zero (Pol [unit]))

eea :: KnownNat n => EEA (Pol n) -> EEA (Pol n) -> EEA (Pol n)
eea (EEA r0 s0 t0) (EEA r1 s1 t1) = let (q1, r2) = division r0 r1 in
  if r2 == zero then (EEA r1 s1 t1) else eea (EEA r1 s1 t1) (EEA r2 (s0 - q1*s1) (t0 - q1*t1))

                      
-- | Degree of polynomial.
deg :: Pol n -> Int
deg (Pol p) = length p - 1

-- | Inverse in F_n of higher-degree coefficient.
inverseLast :: KnownNat n => Pol n -> Maybe (Fn n)
inverseLast (Pol p) = inverse (last p)

-- | Transforms a list of Nat ns to Pol ns where the naturals are taken mod n.
--   m must be (zero :: Fn n). It is needed to specify n easily.
toPol :: KnownNat n => [Nat] -> Fn n -> Pol n
toPol xs m = Pol (map fromIntegral xs)


-- Obtain all the polynomials with coefficients in F_n with degree less or equal than Int.
getAllPolynomials :: KnownNat n => Int -> [Pol n] -> [Pol n]
getAllPolynomials degree pols | degree >  0 = getAllPolynomials (degree - 1) (getPolynomialsNextDegree pols)
                              | degree == 0 = pols
                              | degree < 0 = []

-- Obtain the polynomials of degree d + 1 using the polynomials previousDegree of degree d.
getPolynomialsNextDegree :: KnownNat n => [Pol n]-> [Pol n]
getPolynomialsNextDegree previousDegree = previousDegree >>= \ (Pol p) -> (map (\ x -> Pol (x : p)) getElements)




-- | Implementation of Galois Fields using quotient of polynomial ring.
-- | p must be a list with the coefficients of the polynomial module n.
data GF (p :: NatList) (n :: Nat) = GF (Pol n)
  deriving Show

-- | Eq
instance (KnownList p, KnownNat n) => Eq (GF (p :: NatList) (n :: Nat)) where
  GF q1 == GF q2 = remainder (q1 - q2) pol == zero
    where pol = toPol (val (Proxy :: Proxy p)) (zero :: Fn n)

-- | Num
instance (KnownList p, KnownNat n) => Num (GF (p :: NatList) (n :: Nat)) where
  GF q1 + GF q2 = GF (remainder (q1 + q2) pol)
    where pol = toPol (val (Proxy :: Proxy p)) (zero :: Fn n)
  GF q1 * GF q2 = GF (remainder (q1 * q2) pol)
    where pol = toPol (val (Proxy :: Proxy p)) (zero :: Fn n)
  negate (GF p) = GF (negate p)
  abs x = x
  signum x = GF (Pol [unit])
  fromInteger m = GF (Pol [fromInteger m])

-- | Module
instance (KnownList p, KnownNat n) => Module (GF (p :: NatList) (n :: Nat)) where
  zero = GF zero
  times k (GF p) = GF (times k p)
  
-- | Reducible
instance (KnownList p, KnownNat n) => Reducible (GF (p :: NatList) (n :: Nat)) where
  reduce _ = 0
  getElements = map (\ pol -> GF pol) (getAllPolynomials (deg p) [Pol []])
    where p = toPol (val (Proxy :: Proxy p)) (zero :: Fn n)
  

-- | Field
instance (KnownList p, KnownNat n) => Field (GF (p :: NatList) (n :: Nat)) where
  unit = GF (Pol [unit])
  inverse (GF pol) | (GF pol) == (zero :: (GF p n)) = Nothing
                   | otherwise = let (EEA (Pol q) s t) = startEea (toPol (val (Proxy :: Proxy p)) (zero :: Fn n)) pol
                       in do
                            y <- inverse (head q)
                            return (GF (y `timesF` t))


-- | Aliases for low-characteristic fields.
type F2 = Fn 2
type F3 = Fn 3
type F5 = Fn 5
type F7 = Fn 7
type F11 = Fn 11
type F13 = Fn 13
type F17 = Fn 17

-- | Aliases for polynomials.
type Pol1 = ConsList 1 Void
type Pol2 = ConsList 1 Pol1
type Pol3 = ConsList 1 Pol2
type Pol4 = ConsList 1 Pol3
type Pol5 = ConsList 1 Pol4

-- | Aliases for usual Galois Fields.
--   If the order is a prime p, use Fn p!

-- Char 2
type GF4 = GF Pol3 2
type GF8 = GF Pol4 2
type GF16 = GF Pol5 2

-- Char 3
type GF9 = GF Pol3 3
type GF27 = GF Pol4 3

-- Char 5
type GF25 = GF Pol3 5


                         

        


  

  
  
  
