----
---- CNN-PhD version 0.1, Copyright (C) 25/Jun/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Extra functions to complement CAN
module CANExtras
-- (
-- )
where

import           CANTypes
import           Data.Array.Repa       (computeS, fromFunction, fromListUnboxed,
                                        ix1, ix2, traverse2)
import           Data.Array.Repa.Index ((:.) (..), Z (..))
import           Data.Bool             (bool)
import           ListExtras            (applyNTimes, binaryList, num2Bin',
                                        shiftLeft)

createZNeuron :: Int -> Int -> CANNeuron
createZNeuron rowI colI = CANNeuron (CANWElem initialValue canW0) (CANTElem initialValue canT0)
    where canW0 = createWeight rowI colI []
          canT0 = createThreshold rowI colI []

createWeight :: Int -> Int -> [Int] -> NNTMU
createWeight rowI colI [] = fromListUnboxed (ix2 rowI colI) $ replicate (rowI * colI) False
createWeight rowI colI xs
    | length xs /= (rowI * colI) = createWeight rowI colI []
    | otherwise = fromListUnboxed (ix2 rowI colI) $ binaryList xs

createThreshold :: Int -> Int -> [Int] -> NTTVU
createThreshold rowI colI xs
    | rowI < 0 = computeS $ createZThreshold 0
    | length xs /= rowI = computeS $ createZThreshold rowI
    | null xs = fromListUnboxed (ix1 rowI) $ replicate rowI colI
    | otherwise = fromListUnboxed (ix1 rowI) xs

createZThreshold :: NTT -> NTTVD
createZThreshold rowI
    | rowI <= 0 = fromFunction (ix1 0) (const 0)
    | otherwise = fromFunction (ix1 rowI) (const 0)

createOutput :: Int -> [Int] -> NNTVU
createOutput elems [] = fromListUnboxed (ix1 elems) $ replicate elems False
createOutput elems xs
    | length xs /= elems = createOutput elems []
    | otherwise = fromListUnboxed (ix1 elems) $ binaryList xs

createOutputLst :: Int -> [Int] -> [NNTVU]
createOutputLst _ [] = []
createOutputLst binLength xs = map elemsLst xs
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

constructUpdate :: Int -> [CANUpdate]
constructUpdate nnElems = elems (nnElems - 1)
    where canElems = [CANWeight, CANThreshold]
          elems n = reverse $ concatMap (\x -> map (CANUpdate x) canElems) [0 .. n]
