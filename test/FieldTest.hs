module FieldTest where

import SIDH.Field
import Test.QuickCheck

main :: IO ()
main = do
  _ <- quickCheck inverseTest
  return ()
  
  
inverseTest :: Field a => a -> Bool 
inverseTest x = inverse x * x == unit

getPolsTest :: Int -> [Pol n] -> Bool
getPolsTest degree pols = max (map (\ x -> deg x) getAllPolynomials degree pols) == max (map deg pols) + degree
  

