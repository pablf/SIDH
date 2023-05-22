-----------------------------------------------------------------------------
-- 
-- Module      :  Field
-- Author      :  pablf
--
-- Stability   : Experimental
--
-- Keys of SIDH protocol and encryptation.
--
-- NOTE THAT SIDH PROTOCOL WAS PROVEN NOT SAFE IN 2022!
-- IT CAN'T BE USED FOR SAFE COMMUNICATION!
--
-----------------------------------------------------------------------------

module SIDH.Keys where

import SIDH.EC (EC, Point, veluEC, veluFunction, getKernel, plus, mult)
import SIDH.Field (Field)

-- | An user must have a PublicKey and a PrivateKey. A Key is used to
--   simplify the information. Key and PrivateKey must be secret
--   at all times.

data SIDHKey a = SIDHKey (SIDHPublicKey a) (SIDHPrivateKey a)
  deriving (Eq, Show)


data SIDHPublicKey a = SIDHPublicKey (EC a) (Point a) (Point a)
  deriving (Eq, Show)

data SIDHPrivateKey a = SIDHPrivateKey Int Int
  deriving (Eq, Show)


-- | To operate this protocol you need a Key.
-- | To communicate using this protocol, both endpoints must use the same field a.

-- | If you are A and want to enter the shared secret with B:
--                  1. you must give B (encrypt (Key of A) (PublicKey of B))
--                  2. B must return you a new PublicKey pKeyB2
--                  3. to obtain the shared secret, (encrypt (Key of A) pKeyB2).

-- | If you are A and want to allow B to enter the common shared secret,
--                  1. you must receive a PublicKey pkeyB of B,
--                  2. give (encrypt (Key of A) pKeyB ) to B

-- | encrypt takes the key of A, the public key of B and returns the points of the public key of B in the elliptic curve of A.
encrypt :: Field a => SIDHKey a -> SIDHPublicKey a -> SIDHPublicKey a
encrypt (SIDHKey (SIDHPublicKey ec1 p1 q1) (SIDHPrivateKey n m)) (SIDHPublicKey ec2 p2 q2) =
  SIDHPublicKey (veluEC ec1 kernel) (veluFunction ec1 kernel p2) (veluFunction ec1 kernel q2)
    where kernel = getKernel ec1 (plus ec1 (mult ec1 n p1) (mult ec1 m q1))


