----
---- CNN-PhD version 0.1, Copyright (C) 17/Aug/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Unit test for some functions on CANRandom
module CANRandomTests
-- (
-- )
where

import           CAN
import           CANExtras       (createNTTVU)
import           CANRandom
import           CANTypes
import           Control.Monad   (when)
import qualified Data.Array.Repa as R
import           Data.Bits       (xor)
import           Data.Bool       (bool)
import           Data.List       (zip5)
import           ListExtras      (applyNTimes, shiftLeft)
import           System.Random
import           TestExtras

randomNTTVUTest :: IO ()
randomNTTVUTest = do
    print "randomNTTVUTest"
    let gen = mkStdGen 5
    --- To be tested
    let result = [randomNTTVUOne gen 4 4 [1,1,4,0],
                  randomNTTVUOne gen 4 5 [5,1,1,0],
                  randomNTTVUOne gen 6 5 [5,1,1,0,2,1],
                  randomNTTVUOne gen 0 5 [],
                  -- randomNTTVUOne gen 2 (-1) [],
                  randomNTTVUOne gen (-1) 2 []
                  -- randomNTTVUOne gen (-1) (-1) []
                 ]
    --- Finish testing
    printResult result

--------------------------------------------------------------------------------
---------- Extra Methods ----------
--------------------------------------------------------------------------------

randomNTTVUOne :: RandomGen g => g -> Int -> NTT -> [NTT] -> Bool
randomNTTVUOne gen elems maxVal expected = expV == result
    where result = randomNTTVU gen elems maxVal
          expV = createNTTVU elems 0 expected
