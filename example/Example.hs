{-# LANGUAGE DataKinds #-}

import SIDH.EC
import SIDH.Field
import SIDH.Keys
import SIDH.Protocol

-- | Elliptic Curve
a1 = Fn 2 :: F5
a2 = Fn 1 :: F5
ec = EC a1 a2

-- | Points for SIDH keys
pA = Point a2 a1
qA = Point (Fn 3 :: F5) (Fn 2)

pB = Point a2 a1
qB = Point (Fn 3 :: F5) (Fn 2)

-- | SIDH keys
pubKeyA = SIDHPublicKey ec pA qA
priKeyA = SIDHPrivateKey 7 8
pubKeyB = SIDHPublicKey ec pB qB
priKeyB = SIDHPrivateKey 11 4

keyA = SIDHKey pubKeyA priKeyA
keyB = SIDHKey pubKeyB priKeyB
keyAB :: SIDHPublicKey F5
keyAB = encrypt keyA pubKeyB
keyBA :: SIDHPublicKey F5
keyBA = encrypt keyB pubKeyA


-- | Encrypt and decrypt a message.
main :: IO ()
main = do 
  putStrLn "Enter an Int. It will be encrypted and decrypted using SIDH."
  message    <- getLine
  let messageInt = read message :: Int
      cyphered   = cypher   (SIDH :: (SIDHInt F5)) messageInt keyAB :: Int
      decyphered = decypher (SIDH :: (SIDHInt F5)) cyphered keyBA
  putStrLn $ "Message was: " ++ show messageInt ++ ". Cyphered was: " ++ show cyphered ++ ". Decyphered was: " ++ show decyphered
  putStrLn $ if messageInt == decyphered then "Successfully retrieved!" else "Something failed..."
  

