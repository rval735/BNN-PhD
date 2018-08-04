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

-- | Tri-State Neural Network class
module CAM
-- (
-- )
where

import           CAMExtras                         (construct1Complement,
                                                    constructUpdate)
import           CAMTypes
import           Control.Monad                     (mapM_)
import           Data.Array.Repa
import           Data.Array.Repa.Algorithms.Matrix (col)
import           Data.Array.Repa.Eval              (Target)
import           Data.Array.Repa.Repr.Unboxed      (Unbox)
import           Data.Bits                         (complement, xor, (.&.),
                                                    (.|.))
import           Data.Bool                         (bool)
import           Data.List                         (mapAccumL)
import           Data.Maybe                        (catMaybes, fromMaybe,
                                                    isJust)
import           ListExtras                        (applyNTimes,
                                                    indexFirstNonZeroI,
                                                    replaceElem, safeHead,
                                                    shiftLeft, toBoth)
import           Prelude                           hiding (map, traverse,
                                                    zipWith)
import qualified Prelude                           as P

transformElem :: (Shape sh, Unbox a) => Array U sh a -> sh -> (a -> a)-> Array U sh a
transformElem mtx sh trans = computeS res
    where res = traverse mtx id (\f shape -> bool (f shape) (trans $ f shape) (shape == sh))

layerColide :: CAMWElem -> NNTVU -> (NNT -> NNT -> NNT)-> NNTMU
layerColide (CAMWElem _ mtx) vtr colision = computeS $ traverse2 mtx vtr const applySHY
    where applySHY f h sh@(Z :. x :. y) = colision (f sh) (h $ ix1 y)

layerSummation :: NNTMU -> NTTVU
layerSummation = sumS . map (bool 0 1)

layerOperation :: NTTVU -> CAMTElem -> (NTT -> NTT -> NNT) -> NNTVU
layerOperation x (CAMTElem _ y) f = computeS $ zipWith f x y

applyNeuron :: NNTVU -> CAMNeuron -> NNTVU
applyNeuron input (CAMNeuron camW camT) = res
    where collision = layerColide camW input xor
          summ = layerSummation collision
          res = layerOperation summ camT (>=)

applyReverseNeuron :: NNTVU -> CAMNeuron -> NNTVU
applyReverseNeuron input (CAMNeuron camW camT) = res
    where collision = layerColide camW input (\x y -> complement $ xor x y)
          summ = layerSummation collision
          res = layerOperation summ camT (<)

-- TR stands for Transpose - Reverse
applyTRNeuron :: NNTVU -> CAMNeuron -> NNTVU
applyTRNeuron input (CAMNeuron (CAMWElem reg camW) camT) = res
    where camWE = CAMWElem reg (computeS $ transpose camW)
          collision = layerColide camWE input (\x y -> complement $ xor x y)
          summ = layerSummation collision
          res = layerOperation summ camT (<)

weightsToDelta :: NNTMU -> NNTVU
weightsToDelta = foldS (.|.) False

applyBackProp :: NNTVU -> NNTVU -> NNTVU -> CAMNeuron -> NNTVU
applyBackProp delta input output (CAMNeuron camW camT) = weightsToDelta dWeights
    where dWeights = computeS . transpose $ deltaWeights camW input output delta

vecCompare :: NNTVU -> NNTVU -> NNTVU
vecCompare x y = computeS $ zipWith xor x y

hammingWeight :: NNTVU -> Int
hammingWeight x = sumAllS $ map (bool 0 1) x

queryNeurons :: [CAMNeuron] -> QueryData -> NNTVU
queryNeurons nn query = foldl applyNeuron query nn

queryNeuronsAcc :: [CAMNeuron] -> QueryData -> (NNTVU, [(NNTMU, NNTVU)])
queryNeuronsAcc nn query = mapAccumL f query nn
    where f x n@(CAMNeuron y _) = let apNN = applyNeuron x n in (apNN, (layerColide y x xor, applyNeuron x n))

queryCAMNN :: [CAMNeuron] -> [TrainElem] -> [NNTVU]
queryCAMNN nn = P.map (\(TrainElem query _) -> queryNeurons nn query)

queryReverseNeurons :: [CAMNeuron] -> TrainElem -> NNTVU
queryReverseNeurons nn (TrainElem query _) = foldl applyReverseNeuron query nn

queryReverseCAMNN :: [CAMNeuron] -> [TrainElem] -> [NNTVU]
queryReverseCAMNN nn = P.map (queryReverseNeurons nn)

distanceCAMNN :: [CAMNeuron] -> [TrainElem] -> [Int]
distanceCAMNN nn testSet = compared
    where queries = queryCAMNN nn testSet
          zipped = P.zipWith (\x (TrainElem _ y) -> (x,y)) queries testSet
          compared = P.map (hammingWeight . uncurry vecCompare) zipped

updateCAMNeuron :: CAMNeuron -> CAMElem -> NNTVU -> (NNTMU, NNTVU) -> CAMNeuron
updateCAMNeuron (CAMNeuron camW camT) CAMThreshold deltaP (dW, output) = CAMNeuron camW camTElem
    where dThreshold = deltaThreshold deltaP output
          camTElem = applyDeltaThreshold camT dThreshold (col $ extent dW)
updateCAMNeuron (CAMNeuron camW camT) CAMWeight deltaP wAo = CAMNeuron camWE camT
    where dWeights = deltaWeights' wAo deltaP
          camWE = applyDeltaWeight camW dWeights

calculateDelta :: [(NNTMU, NNTVU)] -> NNTVU -> NNTVU
-- calculateDelta wAo delta = foldl (\y x -> weightsToDelta $ deltaWeights' x y) delta wAo
calculateDelta wAo delta = foldl (\y x -> weightsToDelta . computeS . transpose $ deltaWeights' x y) delta wAo

deltaThreshold :: NNTVU -> NNTVU -> NTTVU
deltaThreshold delta output = computeS $ zipWith changes delta output
    where changes x y = bool 0 (bool (-1) 1 y) x

deltaWeights :: CAMWElem -> NNTVU -> NNTVU -> NNTVU -> NNTMU
deltaWeights (CAMWElem _ weights) input output delta = computeS res
    where applySHY f g h j sh@(Z :. x :. y) = let nSh = ix1 x in applySelection (f sh) (g nSh) (h nSh) (j nSh)
          res = traverse4 weights input output delta (\x _ _ _ -> x) applySHY

deltaWeights' :: (NNTMU, NNTVU) -> NNTVU -> NNTMU
deltaWeights' (wXh, output) delta = computeS res
    where applySHY f g h sh@(Z :. x :. y) = let nSh = ix1 x in applySelection' (f sh) (g nSh) (h nSh)
          res = traverse3 wXh output delta (\x _ _ -> x) applySHY

applyDeltaWeight :: CAMWElem -> NNTMU -> CAMWElem
applyDeltaWeight (CAMWElem wChange weights) delta = CAMWElem updatedIndex camW
    where (updatedIndex, deltaToChange) = deltaNextChange delta wChange
          camW = computeS $ zipWith (\x y -> bool x (complement x) y) weights deltaToChange

deltaNextChange :: NNTMU -> Int -> (Int, NNTMU)
deltaNextChange delta lastWChange = finalDelta
    where oredDelta = foldS (.|.) False $ transpose delta
          changeIndexM = weightIndexChange lastWChange oredDelta
          finalDelta = case changeIndexM of
              Just index -> (index, computeS $ traverse delta id (\f sh@(Z :. x :. y) -> bool False (f sh) (y == index)))
              Nothing    -> (-1, computeS $ map (const False) delta)

applyDeltaThreshold :: CAMTElem -> NTTVU -> Int -> CAMTElem
applyDeltaThreshold cte@(CAMTElem tChange camT) delta maxValue = cteUpdate
    where changeIndexM = thresholdIndexChange tChange delta
          cteUpdate = case changeIndexM of
              Just changeIndex -> CAMTElem changeIndex $ updatedT changeIndex
              Nothing          -> cte
          withinBound x y = let opr = x + y in bool 0 (bool maxValue opr (opr <= maxValue)) (opr >= 0)
          applySHY pos f g sh@(Z :. x) = let update = withinBound (f sh) (g sh) in bool (f sh) update (x == pos)
          updatedT pos = computeS $ traverse2 camT delta const (applySHY pos)

-- Here we change the value to the nearest index change.
-- For example a vector [-1,0,1,0] has two changes/stay differences
-- If "indexChange" is -1, it means it has never changed, then we expect
-- to return Just 0, because index 0 has a change value.
-- If "indexChange" is 0, it means last time 0 was modified, then it should
-- return Just 2, because index 1 does not represent a change.
thresholdIndexChange :: Int -> NTTVU -> Maybe Int
thresholdIndexChange = indexChange (/= 0)

weightIndexChange :: Int -> NNTVU -> Maybe Int
weightIndexChange = indexChange id

indexChange :: (Unbox a) => (a -> Bool) -> Int -> Array U DIM1 a -> Maybe Int
indexChange condition location delta = safeHead $ catMaybes [safeHead y, safeHead x]
    where traversal g sh@(Z :. x) = bool (-1) x (condition $ g sh)
          (x, y) = toBoth (filter (>= 0)) . splitAt (location + 1) . toList $ traverse delta id traversal

applySelection :: NNT -> NNT -> NNT -> NNT -> NNT
applySelection x y z w = xor (xor x y) (complement z) .&. w

applySelection' :: NNT -> NNT -> NNT -> NNT
applySelection' x y z = z .&. xor x (complement y)

---------------------------------------------------------------------------

lstCAMElems :: Int -> [CAMElem]
lstCAMElems elems = [bool CAMWeight CAMThreshold (even x) | x <- [1 .. elems]]

updatesWithConditions :: Int -> Int -> Int -> [CAMUpdate]
updatesWithConditions nnElems trainElems shiftBy
    | (nnElems * 2) < trainElems = transform matchTrain
    | otherwise = transform updates
        where updates = constructUpdate nnElems
              matchTrain = concat $ replicate (div trainElems (nnElems * 2) + 1) updates
              transform = take trainElems . applyNTimes shiftLeft shiftBy

---------------------------------------------------------------------------

trainNeurons :: TrainElem -> CAMUpdate -> [CAMNeuron] -> [CAMNeuron]
trainNeurons (TrainElem train desired) (CAMUpdate lIndex cElem) nn = bool nn' nn (hammingWeight compared == 0)
    where (query, wAo) = queryNeuronsAcc nn train -- wAo = Weights and Outputs
          compared = vecCompare query desired
          chosenNeuron = nn !! lIndex
          wAoI = wAo !! lIndex
          deltaPos = take (length nn - lIndex - 1) $ reverse wAo
          deltaP = calculateDelta deltaPos compared
          atLeastOne = hammingWeight deltaP
          camNN = updateCAMNeuron chosenNeuron cElem deltaP wAoI
          nn' = bool nn (replaceElem nn lIndex camNN) $ atLeastOne > 0

trainCAMNN :: [CAMNeuron] -> [CAMUpdate] -> [TrainElem] -> [CAMNeuron]
trainCAMNN nn updates trainSet = foldl (\n (x, y) -> trainNeurons x y n) nn (zip trainSet updates)

trainUntilLearned :: [CAMNeuron] -> [TrainElem] -> Int -> Int -> IO [CAMNeuron]
trainUntilLearned nn trainSet shift tolerance = do
    let updates = updatesWithConditions (length nn) (length trainSet) shift
    let nn' = trainCAMNN nn updates trainSet
    -- print . filter (uncurry (==)) $ zip nn nn'
    -- printNN nn nn'
    -- print updates
    let distance = sum $ distanceCAMNN nn' trainSet
    print distance
    bool (trainUntilLearned nn' trainSet (shift + 1) tolerance) (return nn') (distance <= tolerance)

printNN :: [CAMNeuron] -> [CAMNeuron] -> IO ()
printNN nn nn' = do
    let zipped = zip nn nn'
    print "---------------"
    mapM_ (\(x,y) -> do print "NN:"; print x; print "NN':"; print y ) zipped
