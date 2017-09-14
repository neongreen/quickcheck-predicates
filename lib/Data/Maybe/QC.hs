module Data.Maybe.QC
(
  isJust,
  isNothing,
)
where


import BasePrelude hiding (isJust, isNothing)
import Test.QuickCheck

import Misc.QC


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
