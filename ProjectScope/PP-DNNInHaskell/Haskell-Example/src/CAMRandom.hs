----
---- CNN-PhD version 0.1, Copyright (C) 24/Jul/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Helper file to add random creation of CAM types
module CAMRandom
-- (
-- )
where

import           CAMTypes
import           Data.Array.Repa                      (fromListUnboxed, ix1,
                                                       ix2)
import           Data.Array.Repa.Algorithms.Randomish (randomishIntArray)
import           System.Random

randomNNTMU :: RandomGen g => g -> WeightsSize -> ThresholdSize -> NNTMU
randomNNTMU seed x y = fromListUnboxed shape lstElems
    where lstElems = take (x * y) $ randoms seed :: [NNT]
          shape = ix2 x y

randomNNTVU :: RandomGen g => g -> ThresholdSize -> NNTVU
randomNNTVU seed y = fromListUnboxed shape lstElems
    where lstElems = take y $ randoms seed :: [NNT]
          shape = ix1 y

randomNTTVU :: RandomGen g => g -> ThresholdSize -> NTTVU
randomNTTVU seed y = randomishIntArray (ix1 y) 0 y (fst $ next seed)

randomCAMNeuron :: RandomGen g => g -> WeightsSize -> ThresholdSize -> CAMNeuron
randomCAMNeuron seed x y = CAMNeuron (CAMWElem (randomNNTMU seed x y) 0) (CAMTElem (randomNTTVU seed' x) 0)
    where (_, seed') = next seed
