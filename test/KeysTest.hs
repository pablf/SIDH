module KeysTest where

import SIDH.Keys
import SIDH.Field
import Arbitraries
import Test.QuickCheck

main :: IO ()
main = do
  _ <- quickCheck encryptTest
  return ()
  
  
encryptTest :: SIDHKey GF25 -> SIDHKey GF25 -> Bool
encryptTest (SIDHKey k1 k2) (SIDHKey k3 k4) = encrypt (SIDHKey k3 k4) (encrypt (SIDHKey k1 k2) k3) == encrypt (SIDHKey k1 k2) (encrypt (SIDHKey k3 k4) k1)
