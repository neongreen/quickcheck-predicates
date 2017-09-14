module Data.Ix.QC
(
  inRange,
)
where


import BasePrelude hiding (inRange)
import Test.QuickCheck
import qualified Data.Ix

import Misc.QC


inRange :: (Ix a, Show a) => (a, a) -> a -> Property
inRange rng a
  | Data.Ix.inRange rng a = success
  | otherwise = failure $
      "expected " <> show a <> " to be in range " <> show rng
