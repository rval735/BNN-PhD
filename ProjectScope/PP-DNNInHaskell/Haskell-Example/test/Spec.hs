----
---- CNN-PhD version 0.1, Copyright (C) 5/Mar/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

import           CANTests

main :: IO ()
main = do
    layerColideTest
    layerSummationTest
    trainCANNNTest_2x2NNXOR
    -- trainCAMNNTest_Ex2NN
    -- trainUntilLearnedTest
    applyDeltaThresholdTest
    applyDeltaWeightTest
    deltaNextChangeTest
    thresholdIndexChangeTest
    weightIndexChangeTest
    splitVecAtTest
    trainWithEpochsTest
