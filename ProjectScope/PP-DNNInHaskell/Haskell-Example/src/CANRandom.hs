----
---- CNN-PhD version 0.1, Copyright (C) 24/Jul/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Helper file to add random creation of CAN types
module CANRandom
-- (
-- )
where

import           CANTypes
import           Data.Array.Repa                      (fromListUnboxed, ix1,
                                                       ix2)
import           Data.Array.Repa.Algorithms.Randomish (randomishIntArray)
import           System.Random                        (RandomGen, next, randoms,
                                                       split)

randomNNTMU :: RandomGen g => g -> Int -> Int -> NNTMU
randomNNTMU seed rowI colI = fromListUnboxed shape lstElems
    where lstElems = take (rowI * colI) $ randoms seed :: [NNT]
          shape = ix2 rowI colI

randomNNTVU :: RandomGen g => g -> Int -> NNTVU
randomNNTVU seed elems = fromListUnboxed shape lstElems
    where lstElems = take elems $ randoms seed :: [NNT]
          shape = ix1 elems

randomNTTVU :: RandomGen g => g -> Int -> Int -> NTTVU
randomNTTVU seed elems maxVal = randomishIntArray (ix1 elems) 0 maxVal (fst $ next seed)

randomCANNeuron :: RandomGen g => g -> Int -> Int -> CANNeuron
randomCANNeuron seed rowI colI = CANNeuron wElem tElem
    where (seed1, seed0) = split seed
          wElem = CANWElem initialValue $ randomNNTMU seed0 rowI colI
          tElem = CANTElem initialValue $ randomNTTVU seed1 rowI colI
