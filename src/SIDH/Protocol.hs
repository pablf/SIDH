-----------------------------------------------------------------------------
-- 
-- Module      :  Field
-- Author      :  pablf
--
-- Stability   : Experimental
--
-- Implementation of a general protocol using any function on the
-- shared secret given by a PublicKey in module Keys.
--
-- Implementation of ElGamal analogue using j-i
--
-----------------------------------------------------------------------------

{-# LANGUAGE UndecidableInstances#-}

module SIDH.Protocol where

import SIDH.Keys
import SIDH.EC (toBinary, jInvariant)
import SIDH.Field (Field, Reducible, reduce)

-- Note that the protocol parameter is needed to have different
-- cypher and decypher depending on different protocols without ambiguity.

-- | Description of a general cryptosystem.
class Protocol plain cyphered key protocol where
  cypher :: protocol -> plain -> key -> cyphered
  decypher :: protocol -> cyphered -> key -> plain

-- | Correspondence between values and keys.
class F val key protocol where
  f :: protocol -> key -> val
  
class Group val where
  plus  :: val -> val -> val
  minus :: val -> val -> val
  
-- | Construct an instance of protocol from any F.
--   It is used with SIDH protocol below
instance (Group val, F val key protocol) => Protocol val val key protocol where
  cypher prot p k   = p `plus`  (f prot k)
  decypher prot c k = c `minus` (f prot k)


  

------------------------
-- | Protocol with SIDH
data SIDH val field = SIDH

-- | Aliases
type SIDHF field = SIDH field field
type SIDHInt field = SIDH Int field
type SIDHBool field = SIDH [Bool] field

-- | Group instances
instance Num a => Group a where
  plus  a b = a + b
  minus a b = a - b
  
instance Group [Bool] where
  plus  m s = plusBool m s
  minus m s = plusBool m s

-- | Sum of Bool list index-wise.
plusBool :: [Bool] -> [Bool] -> [Bool]
plusBool [] [] = []
plusBool [] xs = xs
plusBool xs [] = xs
plusBool (x:xs) (y:ys) = (x /= y) : (plusBool xs ys)


-- | SIDH instances
-- | SIDH using j invariant and field as val
instance Field field => F field (SIDHPublicKey field) (SIDHF field) where
  f _ (SIDHPublicKey ec _ _) = jInvariant ec
  
-- | SIDH using j invariant and Int as val
instance (Reducible field, Field field) => F Int (SIDHPublicKey field) (SIDHInt field) where
  f _ (SIDHPublicKey ec _ _) = reduce (jInvariant ec)

-- | SIDH using j invariant and [Bool] as val
instance (Reducible field, Field field) => F [Bool] (SIDHPublicKey field) (SIDHBool field) where
  f _ (SIDHPublicKey ec _ _) = toBinary (reduce (jInvariant ec))
  

