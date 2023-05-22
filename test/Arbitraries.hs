module Arbitraries where

import SIDH.Field
import SIDH.NatList
import SIDH.EC
import SIDH.Keys
import Test.QuickCheck (Arbitrary, arbitrary)
import GHC.TypeLits

instance (KnownNat n) => Arbitrary (Fn n) where
  arbitrary = do
    x :: Int <- arbitrary
    return (Fn x :: Fn n)

instance (KnownNat n) => Arbitrary (Pol n) where
  arbitrary = do
    (pol :: [Fn n]) <- arbitrary
    return (Pol pol :: Pol n)

instance (KnownNat n, KnownList p) => Arbitrary (GF p n) where
  arbitrary = do
    (pol :: Pol n) <- arbitrary
    return (GF pol :: (GF p n))

instance (Field a, Arbitrary a) => Arbitrary (EC a) where
  arbitrary = do
    x :: a <- arbitrary
    y :: a <- arbitrary
    return (EC x y)

instance (Field a, Arbitrary a) => Arbitrary (Point a) where
  arbitrary = do
    x :: a <- arbitrary
    y :: a <- arbitrary
    return (Point x y)

instance (Field a, Arbitrary a) => Arbitrary (SIDHPublicKey a) where
  arbitrary = do
    ec :: (EC a) <- arbitrary
    p :: (Point a) <- arbitrary
    q :: (Point a) <- arbitrary
    return (SIDHPublicKey ec p q)

instance Arbitrary (SIDHPrivateKey a) where
  arbitrary = do
    x :: Int <- arbitrary
    y :: Int <- arbitrary
    return (SIDHPrivateKey x y)

instance (Field a, Arbitrary a) => Arbitrary (SIDHKey a) where
  arbitrary = do
    publicKey :: (SIDHPublicKey a) <- arbitrary
    privateKey :: (SIDHPrivateKey a) <- arbitrary
    return (SIDHKey publicKey privateKey)

