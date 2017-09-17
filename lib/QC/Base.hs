module QC.Base
(
  -- * Trivial properties
  success,
  failure,

  -- * Combining properties
  (&&), (||), not,
  and, or,
  all, any,

  -- * Comparisons
  (==), (/=),
  (>), (<), (>=), (<=),

  -- * Lists and other foldables
  null,
  elem, notElem,
--  isPrefixOf, isSuffixOf, isInfixOf, isSubsequenceOf,

  -- * Indices
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
  ( Show(..), Eq, Ord, Foldable, Ix
  -- types
  , Bool(..), Either(..), Maybe(..), String
  -- functions
  , ($), (.), (<>), otherwise, map )

import Test.QuickCheck
import qualified Data.Eq       as Eq
import qualified Data.Ord      as Ord
import qualified Data.Foldable as Foldable
import qualified Data.Ix       as Ix


----------------------------------------------------------------------------
-- Trivial
----------------------------------------------------------------------------

failure :: String -> Property
failure s = counterexample s False

success :: Property
success = property True

----------------------------------------------------------------------------
-- Combining and modifying properties
----------------------------------------------------------------------------

infixr 3 &&
infixr 2 ||

-- | Fail if the property succeeds.
not :: Testable prop => prop -> Property
not = expectFailure

(&&) :: (Testable prop1, Testable prop2) => prop1 -> prop2 -> Property
(&&) = (.&&.)

(||) :: (Testable prop1, Testable prop2) => prop1 -> prop2 -> Property
(||) = (.||.)

-- | All properties should pass.
and :: (Testable prop, Foldable f) => f prop -> Property
and = conjoin . Foldable.toList

-- | At least one property should pass.
or :: (Testable prop, Foldable f) => f prop -> Property
or = disjoin . Foldable.toList

all
  :: (Testable prop, Show a, Foldable f)
  => (a -> prop) -> f a -> Property
all f xs =
  counterexample "'all' failed because the property failed \
                 \on at least one value" .
  and . map (\a -> counterexample ("value: " <> show a) (f a)) $
  Foldable.toList xs

any
  :: (Testable prop, Show (f a), Foldable f)
  => (a -> prop) -> f a -> Property
any f xs =
  counterexample "'any' failed because the property failed \
                 \on all values" .
  counterexample ("values: " <> show xs) .
  or . map f $
  Foldable.toList xs

----------------------------------------------------------------------------
-- Comparisons
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
-- Lists & Foldable
----------------------------------------------------------------------------

infix 4 `elem`

null :: (Foldable f, Show (f a)) => f a -> Property
null x
  | Foldable.null x = success
  | otherwise            = failure $
      "expected: an empty value\n" <>
      "     got: " <> show x

elem :: (Foldable f, Eq a, Show a, Show (f a)) => a -> f a -> Property
elem a x
  | Foldable.elem a x = success
  | otherwise         = failure $
      "expected " <> show a <> "to be an element of " <> show x

notElem :: (Foldable f, Eq a, Show a, Show (f a)) => a -> f a -> Property
notElem a x
  | Foldable.notElem a x = success
  | otherwise            = failure $
      "expected " <> show a <> "not to be an element of " <> show x

----------------------------------------------------------------------------
-- Indices
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
