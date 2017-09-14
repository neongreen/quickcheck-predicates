module Data.Maybe.QC
(
  isJust,
  isNothing,
)
where


import BasePrelude hiding (isJust, isNothing)
import Test.QuickCheck


isJust :: Maybe a -> Property
isJust = \case
  Just _  -> property True
  Nothing -> counterexample "expected Just\n\
                            \     got Nothing" False

isNothing :: Show a => Maybe a -> Property
isNothing = \case
  Just a  -> counterexample ("expected Nothing\n\
                             \     got " <> show (Just a)) False
  Nothing -> property True
