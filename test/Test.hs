module Test where

import FieldTest
import ECTest
import KeyTest

main :: IO ()
main = do
  _ <- FieldTest.main
  _ <- ECTest.main
  _ <- KeysTest.main
  return ()
  
