module KeysTest where

import SIDH.Keys
import Test.QuickCheck

main :: IO ()
main = do
  _ <- quickCheck encryptTest
  return ()
  
  
encryptTest :: Field a => SIDHKey a -> SIDHKey a -> Bool
encryptTest (SIDHKey k1 k2) (SIDHKey k3 k4) = encrypt (SIDHKey k3 k4) (encrypt (SIDHKey k1 k2) k3) == encrypt (SIDHKey k1 k2) (encrypt (SIDHKey k3 k4) k1)
