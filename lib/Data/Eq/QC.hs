module Data.Eq.QC
(
  (==),
  (/=),
)
where


import BasePrelude hiding ((==), (/=))
import Test.QuickCheck
import qualified Data.Eq

import Misc.QC


(==) :: (Show a, Eq a) => a -> a -> Property
(==) a b
  | a Data.Eq.== b = success
  | otherwise      = failure $
      "expected two equal values, got different values\n" <>
      "value 1: " <> show a <> "\n" <>
      "value 2: " <> show b

(/=) :: (Show a, Eq a) => a -> a -> Property
(/=) a b
  | a Data.Eq./= b = success
  | otherwise      = failure $
      "expected two different values, got equal values\n" <>
      "value 1 & 2: " <> show a
