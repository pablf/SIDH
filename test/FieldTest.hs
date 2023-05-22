{-# LANGUAGE DataKinds #-}

module FieldTest where

import SIDH.Field
import Test.QuickCheck
import Arbitraries

main :: IO ()
main = do
  _ <- quickCheck inverseTest
  _ <- quickCheck getPolsTest
  _ <- quickCheck eeaTest
  return ()
  
  
inverseTest :: GF25 -> Bool 
inverseTest x = case (inverse x) of
  Just y  -> y * x == unit
  Nothing -> True

getPolsTest :: Int -> [Pol 19] -> Bool
getPolsTest 0 _ = True
getPolsTest degree pols = maximum (map deg (getAllPolynomials degree pols)) == maximum (map deg pols) + degree

eeaTest :: Pol 19 -> Pol 19 -> Bool
eeaTest p q = let (EEA r s t) = startEea p q 
  in r == p*s + q*t
  

