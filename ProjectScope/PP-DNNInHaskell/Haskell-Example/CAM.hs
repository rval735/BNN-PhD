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

import           CAMTypes
import           Control.Monad                (mapM_)
import           Data.Array.Repa
import           Data.Array.Repa.Eval         (Target)
import           Data.Array.Repa.Repr.Unboxed (Unbox)
import           Data.Bits                    (complement, xor, (.&.), (.|.))
import           Data.Bool                    (bool)
import           Data.Maybe                   (catMaybes, fromMaybe)
import           ListExtras                   (applyNTimes, replaceElem,
                                               safeHead, shiftLeft)
import           Prelude                      hiding (map, traverse, zipWith)
import qualified Prelude                      as P

transformElem :: (Shape sh, Unbox a) => Array U sh a -> sh -> (a -> a)-> Array U sh a
transformElem mtx sh trans = computeUnboxedS res
    where res = traverse mtx id (\f shape -> bool (f shape) (trans $ f shape) (shape == sh))

layerColide :: CAMWElem -> NNTVU -> (NNT -> NNT -> NNT)-> NNTMU
layerColide (CAMWElem mtx _) vtr colision = computeUnboxedS $ traverse2 mtx vtr const applySHY
    where applySHY f h sh@(Z :. x :. y) = colision (f sh) (h $ ix1 y)

layerSummation :: NNTMU -> NTTVU
layerSummation mtx = sumS $ map (bool 0 1) mtx

layerOperation :: NTTVU -> CAMTElem -> (NTT -> NTT -> NNT) -> NNTVU
layerOperation x (CAMTElem y _) f = computeS $ zipWith f x y

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
applyTRNeuron input (CAMNeuron (CAMWElem camW reg) camT) = res
    where camWElem = CAMWElem (computeS $ transpose camW) reg
          collision = layerColide camWElem input (\x y -> complement $ xor x y)
          summ = layerSummation collision
          res = layerOperation summ camT (<)

weightsToDelta :: NNTMU -> NNTVU
weightsToDelta = foldS (.|.) False

applyBackProp :: NNTVU -> NNTVU -> NNTVU -> CAMNeuron -> NNTVU
applyBackProp delta input output (CAMNeuron camW camT) = weightsToDelta dWeights
    where dWeights = computeUnboxedS . transpose $ deltaWeights camW input output delta

vecCompare :: NNTVU -> NNTVU -> NNTVU
vecCompare x y = computeS $ zipWith xor x y

hammingWeight :: NNTVU -> Int
hammingWeight x = sumAllS $ map (bool 0 1) x

queryNeurons :: [CAMNeuron] -> QueryData -> NNTVU
queryNeurons nn query = foldl applyNeuron query nn

queryCAMNN :: [CAMNeuron] -> [TrainElem] -> [NNTVU]
queryCAMNN nn = P.map (\(TrainElem query _) -> queryNeurons nn query)

queryReverseNeurons :: [CAMNeuron] -> TrainElem -> NNTVU
queryReverseNeurons nn (TrainElem query _) = foldl applyReverseNeuron query nn

queryReverseCAMNN :: [CAMNeuron] -> [TrainElem] -> [NNTVU]
queryReverseCAMNN nn = P.map (queryReverseNeurons nn)

distanceCAMNN :: [CAMNeuron] -> [TrainElem] -> [Int]
distanceCAMNN nn testSet = P.map hammingWeight compared
    where queries = queryCAMNN nn testSet
          compared = P.map (uncurry vecCompare) $ P.zipWith (\x (TrainElem _ y) -> (x,y)) queries testSet

updateCAMNeuron :: CAMNeuron -> CAMElem -> NNTVU -> NNTVU -> NNTVU -> CAMNeuron
updateCAMNeuron (CAMNeuron camW camT) CAMThreshold delta output _ = CAMNeuron camW camTElem
    where dThreshold = deltaThreshold delta output
          camTElem = applyDeltaThreshold camT dThreshold
updateCAMNeuron (CAMNeuron camW camT) CAMWeight delta output input = CAMNeuron camWElem camT
    where dWeights = deltaWeights camW input output delta
          camWElem = applyDeltaWeight camW dWeights

deltaThreshold :: NNTVU -> NNTVU -> NTTVU
deltaThreshold delta output = computeUnboxedS $ zipWith changes delta output
    where changes x y = bool 0 (bool (-1) 1 y) x

deltaWeights :: CAMWElem -> NNTVU -> NNTVU -> NNTVU -> NNTMU
deltaWeights (CAMWElem weights _) input output delta = computeUnboxedS res
    where applySHY f g h j sh@(Z :. x :. y) = let nSh = ix1 y in applySelection (f sh) (g nSh) (h nSh) (j nSh)
          res = traverse4 weights input output delta (\x _ _ _ -> x) applySHY

applyDeltaWeight :: CAMWElem -> NNTMU -> CAMWElem
applyDeltaWeight (CAMWElem weights wChange) delta = CAMWElem camW updatedIndex
    where oredDelta = weightsToDelta . computeUnboxedS . transpose $ delta
          changeIndex = fromMaybe (wChange + 1) $ weightIndexChange (wChange + 1) oredDelta
          updatedIndex = bool changeIndex 0 (changeIndex > (length . toList $ oredDelta))
          applySHY f g sh@(Z :. x :. y) = let elemI = f sh in bool elemI (bool elemI (complement elemI) (g sh)) (updatedIndex == y)
          camW = computeUnboxedS $ traverse2 weights delta const applySHY

applyDeltaThreshold :: CAMTElem -> NTTVU -> CAMTElem
applyDeltaThreshold (CAMTElem camT tChange) delta = CAMTElem updatedT updatedIndex
    where changeIndex = fromMaybe (tChange + 1) $ thresholdIndexChange (tChange + 1) delta
          updatedIndex = bool changeIndex 0 (changeIndex > (length . toList $ delta))
          zeroOrMore x y = let opr = x + y in bool 0 opr (opr > 0)
          applySHY f g sh@(Z :. x) = let update = zeroOrMore (f sh) (g sh) in bool (f sh) update (x == updatedIndex)
          updatedT = computeUnboxedS $ traverse2 camT delta const applySHY

thresholdIndexChange :: Int -> NTTVU -> Maybe Int
thresholdIndexChange = indexChange (/= 0)

weightIndexChange :: Int -> NNTVU -> Maybe Int
weightIndexChange = indexChange id

indexChange :: (Unbox a) => (a -> Bool) -> Int -> Array U DIM1 a -> Maybe Int
indexChange condition location delta = safeHead $ catMaybes [safeHead y, safeHead x]
    where traversal g sh@(Z :. x) = bool (-1) x (condition $ g sh)
          (x, y) = splitAt location . filter (>= 0) . toList $ traverse delta id traversal

applySelection :: NNT -> NNT -> NNT -> NNT -> NNT
applySelection x y z w = xor (xor x y) (complement z) .&. w

---------------------------------------------------------------------------

lstCAMElems :: Int -> [CAMElem]
lstCAMElems elems = [bool CAMWeight CAMThreshold (even x) | x <- [1 .. elems]]

constructUpdate :: Int -> [CAMUpdate]
constructUpdate nnElems = reverse $ concatMap (\x -> P.map (CAMUpdate x) camElems) [0 .. count]
    where count = nnElems - 1
          camElems = [CAMWeight, CAMThreshold]

---------------------------------------------------------------------------

trainNeurons :: TrainElem -> CAMUpdate -> [CAMNeuron] -> [CAMNeuron]
trainNeurons (TrainElem train desired) (CAMUpdate lIndex cElem) nn = bool nn' nn (hammingWeight compared == 0)
    where query = queryNeurons nn train
          compared = vecCompare query desired
          chosenNeuron = nn !! lIndex
          choppedNNR = take (length nn - lIndex - 1) $ reverse nn
          choppedNN = take lIndex nn
          hidden = queryNeurons choppedNN train
          output = queryNeurons [chosenNeuron] hidden
          deltaP = foldl applyTRNeuron compared choppedNNR
          camNN = updateCAMNeuron chosenNeuron cElem deltaP output hidden
          nn' = replaceElem nn lIndex camNN

trainCAMNN :: [CAMNeuron] -> [CAMUpdate] -> [TrainElem] -> [CAMNeuron]
trainCAMNN nn updates trainSet = foldl (\n (x, y) -> trainNeurons x y n) nn (zip trainSet updates)

trainUntilLearned :: [CAMNeuron] -> [TrainElem] -> Int -> Int -> IO [CAMNeuron]
trainUntilLearned nn trainSet shiftUpdate tolerance = do
    let updates = constructUpdate $ length nn
    let shifted = take (length trainSet) $ applyNTimes updates shiftLeft shiftUpdate
    let nn' = trainCAMNN nn updates trainSet
    -- print . filter (uncurry (==)) $ zip nn nn'
    -- printNN nn nn'
    print nn'
    let distance = sum $ distanceCAMNN nn' trainSet
    -- print distance
    bool (trainUntilLearned nn' trainSet (shiftUpdate + 1) tolerance) (return nn') (distance <= tolerance)

printNN :: [CAMNeuron] -> [CAMNeuron] -> IO ()
printNN nn nn' = do
    let zipped = zip nn nn'
    print "---------------"
    mapM_ (\(x,y) -> print $ show x P.++ "\n->" P.++ show y ) zipped

