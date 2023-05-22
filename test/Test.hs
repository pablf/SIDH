module Main where

import FieldTest
import ECTest
import KeysTest

main :: IO ()
main = do
  _ <- FieldTest.main
  _ <- ECTest.main
  _ <- KeysTest.main
  return ()
  
