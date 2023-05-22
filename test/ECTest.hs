{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ECTest where

import SIDH.EC
import SIDH.Field
import Test.QuickCheck
import GHC.TypeLits
import Arbitraries

main :: IO ()
main = do
  _ <- quickCheck plusDoubleTest
  return()
  
  
plusDoubleTest :: EC GF25 -> Point GF25 -> Point GF25 -> Bool
plusDoubleTest ec p q = if pertains ec p && pertains ec q then double ec (plus ec p q) == plus ec (double ec p) (double ec q) else True


