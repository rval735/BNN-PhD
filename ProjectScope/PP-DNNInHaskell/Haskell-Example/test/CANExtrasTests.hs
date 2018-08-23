----
---- CNN-PhD version 0.1, Copyright (C) 16/Aug/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Unit test for some critical functions of the CAN file
module CANExtrasTests
-- (
-- )
where

import           CAN
import           CANExtras       (createNTTVU)
import           CANTypes
import           Control.Monad   (when)
import qualified Data.Array.Repa as R
import           Data.Bits       (xor)
import           Data.Bool       (bool)
import           Data.List       (zip5)
import           ListExtras      (applyNTimes, shiftLeft)
import           TestExtras

createThresholdTest :: IO ()
createThresholdTest = do
    print "createThresholdTest"
    --- To be tested
    let result = [createThresholdOne 2 8 [] $ R.fromListUnboxed (R.ix1 2) [8,8],
                  createThresholdOne 2 0 [] $ R.fromListUnboxed (R.ix1 2) [0,0],
                  createThresholdOne 1 1 [2] $ R.fromListUnboxed (R.ix1 1) [2],
                  createThresholdOne 3 9 [1,1,1] $ R.fromListUnboxed (R.ix1 3) [1,1,1],
                  createThresholdOne 2 2 [0,1,2] $ R.fromListUnboxed (R.ix1 2) [0,0],
                  createThresholdOne 4 2 [0,1,2] $ R.fromListUnboxed (R.ix1 4) [0,0,0,0],
                  createThresholdOne 3 2 [0,1,2] $ R.fromListUnboxed (R.ix1 3) [0,1,2]]
    --- Finish testing
    printResult result



--------------------------------------------------------------------------------
---------- Extra Methods ----------
--------------------------------------------------------------------------------

createThresholdOne :: Int -> NTT -> [NTT] -> NTTVU -> Bool
createThresholdOne rowI colI lst expected = result == expected
    where result = createNTTVU rowI colI lst
