module QC.Base
(
  -- * Trivial properties
  success,
  failure,
  
  -- * "Prelude"
  (==), (/=),
  (>), (<), (>=), (<=),
  
  -- * "Data.Ix"
  inRange,
  
  -- * Constructors
  -- ** 'Maybe'
  isNothing,
  isJust,
  -- ** 'Either'
  isLeft,
  isRight,
)
where


import BasePrelude
  -- classes
  ( Show(..), Eq, Ord, Ix
  -- types
  , Bool(..), Either(..), Maybe(..), String
  -- functions
  , ($), (<>), otherwise )

import Test.QuickCheck
import qualified Data.Eq       as Eq
import qualified Data.Ord      as Ord
import qualified Data.Ix       as Ix


----------------------------------------------------------------------------
-- Trivial
----------------------------------------------------------------------------

failure :: String -> Property
failure s = counterexample s False

success :: Property
success = property True

----------------------------------------------------------------------------
-- Prelude
----------------------------------------------------------------------------

infix 4 ==, /=, >, <, >=, <=

(==) :: (Show a, Eq a) => a -> a -> Property
(==) a b
  | a Eq.== b = success
  | otherwise = failure $
      "expected a == b, but got different values\n" <>
      "a: " <> show a <> "\n" <>
      "b: " <> show b

(/=) :: (Show a, Eq a) => a -> a -> Property
(/=) a b
  | a Eq./= b = success
  | otherwise = failure $
      "expected a /= b, but got equal values\n" <>
      "a, b: " <> show a

(>) :: (Show a, Ord a) => a -> a -> Property
(>) a b
  | a Ord.> b = success
  | otherwise = failure $
      "expected a > b\n" <>
      "a: " <> show a <> "\n" <>
      "b: " <> show b

(<) :: (Show a, Ord a) => a -> a -> Property
(<) a b
  | a Ord.< b = success
  | otherwise = failure $
      "expected a < b\n" <>
      "a: " <> show a <> "\n" <>
      "b: " <> show b

(>=) :: (Show a, Ord a) => a -> a -> Property
(>=) a b
  | a Ord.>= b = success
  | otherwise  = failure $
      "expected a >= b\n" <>
      "a: " <> show a <> "\n" <>
      "b: " <> show b

(<=) :: (Show a, Ord a) => a -> a -> Property
(<=) a b
  | a Ord.<= b = success
  | otherwise  = failure $
      "expected a <= b\n" <>
      "a: " <> show a <> "\n" <>
      "b: " <> show b

----------------------------------------------------------------------------
-- Data.Ix
----------------------------------------------------------------------------

inRange :: (Ix a, Show a) => (a, a) -> a -> Property
inRange rng a
  | Ix.inRange rng a = success
  | otherwise        = failure $
      "expected " <> show a <> " to be in range " <> show rng

----------------------------------------------------------------------------
-- Constructors
----------------------------------------------------------------------------

-- Maybe

isJust :: Maybe a -> Property
isJust = \case
  Just _  -> success
  Nothing -> failure "expected: Just\n\
                     \     got: Nothing"

isNothing :: Show a => Maybe a -> Property
isNothing = \case
  Nothing -> success
  Just a  -> failure $ "expected: Nothing\n\
                       \     got: " <> show (Just a)

-- Either

isLeft :: forall a b. Show b => Either a b -> Property
isLeft = \case
  Left _  -> success
  Right b -> failure $ "expected: Left\n\
                       \     got: " <> show (Right b :: Either () b)

isRight :: forall a b. Show a => Either a b -> Property
isRight = \case
  Right _ -> success
  Left a  -> failure $ "expected: Right\n\
                       \     got: " <> show (Left a :: Either a ())
