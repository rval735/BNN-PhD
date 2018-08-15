----
---- CNN-PhD version 0.1, Copyright (C) 5/Jun/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Binary Neural Network that uses an arrange called Content Addressaable Memory
--   to match binary patterns form the input to provide a binary output.
module CAN
-- (
-- )
where

import           CANExtras                         (construct1Complement,
                                                    constructUpdate)
import           CANTypes
import           Control.Monad                     (mapM_, when)
import           Data.Array.Repa
import           Data.Array.Repa.Algorithms.Matrix (col)
import           Data.Array.Repa.Repr.Unboxed      (Unbox)
import           Data.Bits                         (complement, xor, (.&.),
                                                    (.|.))
import           Data.Bool                         (bool)
import           Data.List                         (mapAccumL)
import           Data.Maybe                        (isNothing)
import qualified Data.Vector.Unboxed               as V
import           ListExtras                        (applyNTimes, replaceElem,
                                                    safeHead, shiftLeft, toBoth)
import           Prelude                           hiding (map, traverse,
                                                    zipWith)
import qualified Prelude                           as P

layerColide :: (Source r NNT) => CANWElem -> NNTVF r -> (NNT -> NNT -> NNT)-> NNTMD
layerColide (CANWElem _ mtx) vtr colision = traverse2 mtx vtr const applySHY
    where applySHY f h sh@(Z :. x :. y) = colision (f sh) (h $ ix1 y)

layerSummation :: (Source r NNT) => NNTMF r -> NTTVU
layerSummation = sumS . map (bool 0 1)

layerOperation :: NTTVD -> CANTElem -> (NTT -> NTT -> NNT) -> NNTVD
layerOperation x (CANTElem _ y) f = zipWith f x y

applyNeuron :: (Source r NNT) => NNTVF r -> CANNeuron -> NNTVD
applyNeuron input (CANNeuron canW canT) = res
    where collision = layerColide canW input xor
          summ = layerSummation collision
          res = layerOperation (delay summ) canT (>=)

-- weightsToDelta :: NNTMD -> NNTVD
-- weightsToDelta delta = traverse2 delta  id (\f (Z :. x :. y) -> ) -- foldS (.|.) False

weightsToDelta :: (Source r NNT) => NNTMF r -> NNTVU
weightsToDelta = foldS (.|.) False

vecCompare :: (Source r NNT) => NNTVF r -> NNTVF r -> NNTVD
vecCompare = zipWith xor

hammingWeight :: (Source r NNT) => NNTVF r -> Int
hammingWeight x = sumAllS $ map (bool 0 1) x

queryNeurons :: [CANNeuron] -> NNTVD -> NNTVD
queryNeurons nn query = foldl applyNeuron query nn

queryNeuronsAcc :: [CANNeuron] -> NNTVD -> (NNTVD, [(NNTMD, NNTVD)])
queryNeuronsAcc nn query = mapAccumL f query nn
    where f x n@(CANNeuron y _) = let apNN = applyNeuron x n in (apNN, (layerColide y x xor, applyNeuron x n))

queryCANNN :: [CANNeuron] -> [TrainElem] -> [NNTVD]
queryCANNN nn = P.map (\(TrainElem query _) -> queryNeurons nn (delay query))

distanceCANNN :: [CANNeuron] -> [TrainElem] -> [Int]
distanceCANNN nn testSet = compared
    where queries = queryCANNN nn testSet
          zipped = P.zipWith (\x (TrainElem _ y) -> (x, delay y)) queries testSet
          compared = P.map (hammingWeight . uncurry vecCompare) zipped

updateCANNeuron :: CANNeuron -> CANElem -> NNTVU -> (NNTMD, NNTVD) -> CANNeuron
updateCANNeuron (CANNeuron canW canT) CANThreshold deltaP (dW, output) = CANNeuron canW canTElem
    where dThreshold = deltaThreshold deltaP output
          canTElem = applyDeltaThreshold canT dThreshold (col $ extent dW)
updateCANNeuron (CANNeuron canW canT) CANWeight deltaP (dW, output) = CANNeuron canWE canT
    where dWeights = deltaWeights deltaP (delay dW, delay output)
          canWE = applyDeltaWeight canW dWeights

calculateDelta :: NNTVD -> [(NNTMD, NNTVD)] -> NNTVU
calculateDelta delta = foldl (\x y -> weightsToDelta . transpose $ deltaWeights x y) (computeS delta)

deltaThreshold :: NNTVU -> NNTVD -> NTTVD
deltaThreshold = zipWith changes
    where changes x y = bool 0 (bool (-1) 1 y) x

-- deltaWeights ::  (Source r NNT) => (NNTMF r, NNTVF r) -> NNTVF r -> NNTMD
deltaWeights :: (Source r NNT) => NNTVF r -> (NNTMD, NNTVD) -> NNTMD
deltaWeights delta (wXh, output) = traverse3 wXh output delta (\x _ _ -> x) applySHY
    where applySHY f g h sh@(Z :. x :. y) = let nSh = ix1 x in applySelection (f sh) (g nSh) (h nSh)

applyDeltaWeight :: CANWElem -> NNTMD -> CANWElem
applyDeltaWeight (CANWElem wChange weights) delta = CANWElem updatedIndex canW
    where (updatedIndex, deltaToChange) = deltaNextChange delta wChange
          canW = computeS $ zipWith (\x y -> bool x (complement x) y) weights deltaToChange

deltaNextChange :: NNTMD -> Int -> (Int, NNTMD)
deltaNextChange delta lastWChange = finalDelta
    where oredDelta = foldS (.|.) False $ transpose delta
          changeIndexM = weightIndexChange lastWChange (delay oredDelta)
          finalDelta = case changeIndexM of
              Just index -> (index, traverse delta id (\f sh@(Z :. x :. y) -> bool False (f sh) (y == index)))
              Nothing    -> (initialValue, map (const False) delta)

applyDeltaThreshold :: CANTElem -> NTTVD -> Int -> CANTElem
applyDeltaThreshold cte@(CANTElem tChange canT) delta maxValue = cteUpdate
    where changeIndexM = thresholdIndexChange tChange delta
          cteUpdate = case changeIndexM of
              Just changeIndex -> CANTElem changeIndex $ updatedT changeIndex
              Nothing          -> cte
          withinBound x y = let opr = x + y in bool 0 (bool maxValue opr (opr <= maxValue)) (opr >= 0)
          applySHY pos f g sh@(Z :. x) = let update = withinBound (f sh) (g sh) in bool (f sh) update (x == pos)
          updatedT pos = computeS $ traverse2 canT delta const (applySHY pos)

-- Here we change the value to the nearest index change.
-- For example a vector [-1,0,1,0] has two changes/stay differences
-- If the first argument is "-1", it means it has never been changed, then we expect
-- to return "Just 0", because index "0" has a change value.
-- In case it is "0", it means last time "0" was modified, then it should
-- return "Just 2", because index 1 does not represent a change.
thresholdIndexChange :: Int -> NTTVD -> Maybe Int
thresholdIndexChange = indexChange (/= 0)

weightIndexChange :: Int -> NNTVD -> Maybe Int
weightIndexChange = indexChange id

indexChange :: (Unbox a) => (a -> Bool) -> Int -> Array D DIM1 a -> Maybe Int
indexChange condition location delta = bool y x $ isNothing y
    where traversal g sh@(Z :. x) = bool (-1) x (condition $ g sh)
          splitted = splitVecAt location $ traverse delta id traversal
          (x, y) = toBoth (firstElem . filterVec (>= 0)) splitted


applySelection :: NNT -> NNT -> NNT -> NNT
applySelection x y z = z .&. xor x (complement y)

---------------------------------------------------------------------------

updatesWithConditions :: Int -> Int -> Int -> [CANUpdate]
updatesWithConditions nnElems trainElems shiftBy
    | (nnElems * 2) < trainElems = transform matchTrain
    | otherwise = transform updates
    where updates = constructUpdate nnElems
          matchTrain = concat $ replicate (div trainElems (nnElems * 2) + 1) updates
          transform = take trainElems . applyNTimes shiftLeft shiftBy

---------------------------------------------------------------------------

trainNeurons :: TrainElem -> CANUpdate -> [CANNeuron] -> [CANNeuron]
trainNeurons (TrainElem train desired) (CANUpdate lIndex cElem) nn = bool nn' nn (hammingWeight compared == 0)
    where (query, wAo) = queryNeuronsAcc nn (delay train) -- wAo = Weights and Outputs
          compared = vecCompare query (delay desired)
          chosenNeuron = nn !! lIndex
          wAoI = wAo !! lIndex
          deltaPos = take (length nn - lIndex - 1) $ reverse wAo
          deltaP = calculateDelta compared deltaPos
          atLeastOne = hammingWeight deltaP
          canNN = updateCANNeuron chosenNeuron cElem deltaP wAoI
          nn' = bool nn (replaceElem nn lIndex canNN) $ atLeastOne > 0

trainCANNN :: [CANNeuron] -> [CANUpdate] -> [TrainElem] -> [CANNeuron]
trainCANNN nn updates trainSet = foldl (\n (x, y) -> trainNeurons x y n) nn (zip trainSet updates)

trainUntilLearned :: [CANNeuron] -> [TrainElem] -> Int -> Int -> IO [CANNeuron]
trainUntilLearned nn trainSet shift tolerance = do
    let (shiftTo, nn') = trainGeneral nn trainSet shift
    let distance = sum $ distanceCANNN nn' trainSet
    let printOpr = print nn' >> print ("Distance: " P.++ show distance)
    -- when (shiftTo == 0) printOpr
    bool (trainUntilLearned nn' trainSet shiftTo tolerance) (return nn') (distance <= tolerance)

trainWithEpochs :: [CANNeuron] -> [TrainElem] -> Int -> Int -> IO [CANNeuron]
trainWithEpochs nn _ _ 0 = return nn
trainWithEpochs nn trainSet shift epochs
    | epochs < 0 = return nn
    | otherwise = do
        let (shiftTo, nn') = trainGeneral nn trainSet shift
        -- print $ "Epoch:" P.++ show epochs
        -- print nn'
        trainWithEpochs nn' trainSet shiftTo $ epochs - 1

trainGeneral :: [CANNeuron] -> [TrainElem] -> Int -> (Int, [CANNeuron])
trainGeneral [] _ _ = (initialValue, [])
trainGeneral nn trainSet shiftV = (shiftTo, nn')
    where nnL = fromIntegral $ length nn
          tsL = fromIntegral $ length trainSet
          updates = updatesWithConditions nnL tsL shiftV
          upL = fromIntegral $ length updates
          nn' = trainCANNN nn updates trainSet
          shiftBy = shiftV + bool (tsL - (nnL * 2)) ((nnL * 2) - tsL) (nnL * 2 > tsL)
          shiftTo = bool shiftBy 0 (shiftBy > tsL)

---------------------------------------------------------------------------

splitVecAt :: Int -> NTTVD -> (NTTVD, NTTVD)
splitVecAt location vec
    | location >= size shVec = (vec, emptyVec)
    | location < 0 = (emptyVec, vec)
    | otherwise = (leftV, rightV)
    where shVec = extent vec
          emptyVec = fromFunction shVec (const initialValue)
          indexed = fromIndex shVec location
          locPlus = location + 1
          leftV = extract (ix1 0) (ix1 locPlus) vec
          rightV = extract (ix1 locPlus) (ix1 $ size shVec - locPlus) vec

filterVec :: (Int -> Bool) -> NTTVD -> NTTVU
filterVec filterF vec = fromUnboxed (ix1 $ V.length elems) elems
    where shVec = extent vec
          elems = V.filter filterF . toUnboxed $ computeUnboxedS vec

firstElem :: NTTVU -> Maybe Int
firstElem vec
    | shSize <= 0 = Nothing
    | otherwise = Just (linearIndex vec 0)
    where shSize = size $ extent vec
