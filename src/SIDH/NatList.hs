-----------------------------------------------------------------------------
-- |
-- module      :  NatList
-- Author      :  pablf
--
-- It implements KnownList as an analogue to KnownNat. NatList is a list
-- of Nat and KnownList allows to access the naturals of the list when
-- the list is promoted to type.
--
-- It is used in module Field has accesible list of Nats.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}

module SIDH.NatList where

import GHC.TypeLits
import Data.Proxy

-- | NatList represent a value [Nat]
data NatList = Void | ConsList Nat NatList 

-- | Use a proxy to extract value.
class KnownList l where
  val :: Proxy l -> [Natural]

instance KnownList Void where
  val Proxy = []

instance (KnownNat n, KnownList ns) => KnownList (ConsList n ns) where
  val Proxy = toNat( fromIntegral (natVal (Proxy :: Proxy n))) : val (Proxy :: Proxy ns)


-- | from Int to Nat. If n < 0 it returns 0.
toNat :: Int -> Nat
toNat n | n <= 0 = 0 :: Nat
        | n > 0 = (1 :: Nat) + toNat (n - 1)
