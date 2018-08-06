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
import           ListExtras            (applyNTimes, binaryList, num2Bin',
                                        shiftLeft)
import           System.Random         (mkStdGen, next, randomRs, randoms)

createZNeuron :: Int -> Int -> CAMNeuron
createZNeuron rowI colI = CAMNeuron (CAMWElem (-1) camW0) (CAMTElem (-1) camT0)
    where camW0 = createWeight rowI colI []
          camT0 = createThreshold rowI colI []

createRNeuron :: Int -> Int -> Int -> CAMNeuron
createRNeuron rowI colI seed = CAMNeuron (CAMWElem (-1) camW0) (CAMTElem (-1) camT0)
    where stdGen = mkStdGen seed
          (_, stdGen') = next stdGen
          numberElems = rowI * colI
          camW0 = createWeight rowI colI . take numberElems $ randomRs (0, 1) stdGen
          camT0 = createThreshold rowI 0 . take rowI $ randomRs (0, colI) stdGen'

createWeight :: Int -> Int -> [Int] -> NNTMU
createWeight rowI colI [] = fromListUnboxed (ix2 rowI colI) $ replicate (rowI * colI) False
createWeight rowI colI xs
    | length xs /= (rowI * colI) = createWeight rowI colI []
    | otherwise = fromListUnboxed (ix2 rowI colI) $ binaryList xs

createThreshold :: Int -> Int -> [Int] -> NTTVU
createThreshold rowI colI [] = fromListUnboxed (ix1 rowI) $ replicate rowI colI
createThreshold rowI _ xs
    | length xs /= rowI = createThreshold rowI 0 []
    | otherwise = fromListUnboxed (ix1 rowI) xs

createOutput :: Int -> [Int] -> NNTVU
createOutput elems [] = fromListUnboxed (ix1 elems) $ replicate elems False
createOutput elems xs
    | length xs /= elems = createOutput elems []
    | otherwise = fromListUnboxed (ix1 elems) $ binaryList xs

createOutput' :: Int -> [Int] -> [NNTVU]
createOutput' _ [] = []
createOutput' binLength xs = map elemsLst xs
    where elemsLst = fromListUnboxed (ix1 binLength) . reverse . num2Bin' binLength

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
