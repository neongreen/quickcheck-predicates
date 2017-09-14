module Data.Either.QC
(
  isLeft,
  isRight,
)
where


import BasePrelude hiding (isLeft, isRight)
import Test.QuickCheck

import Misc.QC


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
