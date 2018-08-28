----
---- CNN-PhD version 0.1, Copyright (C) 24/Aug/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Unit test for some critical functions of the CAN file
module CANTypesStorableTests
-- (
-- )
where

import           CAN
import           CANExtras            (constructUpdate, createNNTMU,
                                       createNNTMU', createNNTVU, createNNTVU',
                                       createNNTVULst, createNTTVU,
                                       createZNeuron)
import           CANTypes
import           CANTypesStorable
import           Control.Monad        (when)
import qualified Data.Array.Repa      as R
import           Data.Bits            (xor)
import           Data.Bool            (bool)
import           Data.List            (zip5)
import qualified Data.Vector.Storable as VS
import           ListExtras           (applyNTimes, shiftLeft)
import           TestExtras           (printResult)

nntmuStorableTest :: IO ()
nntmuStorableTest = do
    print "nntmuStorableTest"
    let nttmu = createNNTMU' 3 2 [0,1,0,1,0,1]
    let vss = VS.singleton nttmu
    --- To be tested
    let result = [VS.length vss == 1]
    --- Finish testing
    printResult result
