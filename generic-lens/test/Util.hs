module Util where

import           Test.HUnit.Base
import           Test.Inspection

mkHUnitTest :: Result -> Test
mkHUnitTest r = TestCase $
  case r of
    Success _s -> return ()
    Failure s  -> assertFailure s

