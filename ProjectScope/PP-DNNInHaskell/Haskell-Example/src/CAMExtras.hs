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
import           Data.Array.Repa       (computeS, fromListUnboxed, ix1, ix2,
                                        traverse2)
import           Data.Array.Repa.Index
import           Data.Bool
import           ListExtras            (applyNTimes, shiftLeft)

createZNeuron :: Int -> Int -> CAMNeuron
createZNeuron rowI colI = CAMNeuron (CAMWElem 0 camW0) (CAMTElem 0 camT0)
    where camW0 = fromListUnboxed (ix2 rowI colI) $ replicate (rowI * colI) False
          camT0 = fromListUnboxed (ix1 rowI) $ replicate rowI colI

createWeight :: [Int] -> Int -> Int -> NNTMU
createWeight [] rowI colI = camWElem . camWeights $ createZNeuron rowI colI
createWeight lst rowI colI
    | length lst /= (rowI * colI) = camWElem . camWeights $ createZNeuron rowI colI
    | otherwise = fromListUnboxed (ix2 rowI colI) binLst
    where binLst = map (\x -> bool False True (0 /= x)) lst

construct1Complement :: Int -> Int -> NNTMU
construct1Complement startPos rows
    | startPos > rows = emptyArr
    | startPos < 0 = emptyArr
    | otherwise = computeS $ traverse2 emptyArr indexesArr const flipIndexes
    where emptyArr = fromListUnboxed (ix2 rows rows) $ replicate (rows * rows) False
          indexesLst = applyNTimes shiftLeft startPos [0 .. (rows - 1)]
          indexesArr = fromListUnboxed (ix1 rows) indexesLst
          flipIndexes f g sh@(Z :. x :. y) = let val = g (ix1 x) in bool False True (val == y)

constructUpdate :: Int -> [CAMUpdate]
constructUpdate nnElems = elems (nnElems - 1)
    where camElems = [CAMWeight, CAMThreshold]
          elems n = reverse $ concatMap (\x -> map (CAMUpdate x) camElems) [0 .. n]
