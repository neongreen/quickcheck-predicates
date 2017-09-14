-- | Miscellaneous properties.
module Misc.QC
(
  failure,
  success,
)
where


import Prelude hiding (fail)
import Test.QuickCheck


failure :: String -> Property
failure s = counterexample s False

success :: Property
success = property True
