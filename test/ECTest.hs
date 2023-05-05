module ECTest where

import SIDH.EC
import SIDH.Field
import Test.QuickCheck

main :: IO ()
main = do
  _ <- quickCheck plusDoubleTest
  return()
  
  
plusDoubleTest :: Field a => EC a -> Point a -> Point a-> Bool
plusDoubleTest ec p q = if pertains ec p && pertains ec q then double ec (plus ec p q) == plus ec (double ec p) (double ec q) else True 
