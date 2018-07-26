----
---- CNN-PhD version 0.1, Copyright (C) 25/Jun/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Extra functions to complement CAM
module CAMExtras
-- (
-- )
where

import           CAMTypes
import           Data.Array.Repa (fromListUnboxed, ix1, ix2)

createZNeuron :: Int -> Int -> CAMNeuron
createZNeuron row col = CAMNeuron (CAMWElem camW0 0) (CAMTElem camT0 0)
    where camW0 = fromListUnboxed (ix2 row col) $ replicate (row * col) False
          camT0 = fromListUnboxed (ix1 row) $ replicate row col
