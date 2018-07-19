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


import           Data.Array.Repa                      hiding ((++))
import           Data.Array.Repa.Algorithms.Randomish (randomishIntArray)
import           Data.Array.Repa.Eval                 (Target)
import           Data.Array.Repa.Repr.Unboxed         (Unbox)
import           Data.Bits
import           Data.Bool
import           Data.Word
import           Prelude                              hiding (map, traverse,
                                                       zipWith)
import qualified Prelude                              as P
import           System.Random

type WeightsSize = Int
type ThresholdSize = Int

-- | Code synonyms to ease relationship between function
--   parameters and their application

-- | NNT stands for Neural Network Type, which is the NN
--   type of values it carries and outputs
type NNT = Bool

-- | NTT stands for Neural Threshold Type, which is the NN
--   type of values it needs to consider threshold values
type NTT = Int


-- | Synonyms for dimensions
--   VShape stands for Vector Shape, which is a single dimension
type VShape = DIM1 -- Z :. InputNodes
--   MShape stands for Matrix Shape, which is two dimensions
type MShape = DIM2 -- Z :. InputNodes :. OutputNodes

-- | Synonyms for Array types used in NNClass
--   Flex means that it can be (un)boxed and different shape, but with NNT values
type NNTLayerFlex r sh = Array r sh NNT
type NNTLayerU sh = NNTLayerFlex U sh
type NNTLayerD sh = NNTLayerFlex D sh

-- | NNT - Neural Network Type; V - Vector; D - Delayed
type NNTVD = NNTLayerD VShape
-- | M - Matrix; D - Delayed
type NNTMD = NNTLayerD MShape
-- | U - Unboxed
type NNTVU = NNTLayerU VShape
type NNTMU = NNTLayerU MShape
-- | F - Flexible to be delayed or unboxed
type NNTVF r = NNTLayerFlex r VShape
type NNTMF r = NNTLayerFlex r MShape

type NTTVU = Array U VShape NTT

data CAMNeuron = CAMNeuron {
    camWeights    :: NNTMU,
    camThresholds :: NTTVU
} deriving (Eq, Show)

data CAMWElem = CAMWElem {
    camWElem :: NNTMU,
    wChange :: Int
}

data CAMTElem = CAMWElem {
    camTElem :: NNTMU,
    tChange :: Int
}

type CAMNN = [CAMNeuron]

data CAMElem = CAMWeight | CAMThreshold
    deriving (Eq, Show)

data CAMUpdate = CAMUpdate {
    lstIndex :: Int,
    nnIndex  :: Int,
    camElem  :: CAMElem
} deriving (Show)

data TrainElem = TrainElem {
    trainInput  :: NNTVU,
    trainOutput :: NNTVU
} deriving (Show)

type QueryData = NNTVU

transformElem :: (Shape sh, Bits a, Unbox a) => Array U sh a -> sh -> (a -> a)-> Array U sh a
transformElem mtx sh trans = computeUnboxedS res
    where res = traverse mtx id (\f shape -> bool (f shape) (trans $ f shape) (shape == sh))

layerColide :: NNTMU -> NNTVU -> (NNT -> NNT -> NNT)-> NNTMU
layerColide mtx vtr colision = computeUnboxedS $ traverse2 mtx vtr const applySHY
    where applySHY f h sh@(Z :. x :. y) = colision (f sh) (h $ ix1 y)

layerSummation' :: NNTMU -> NTTVU
layerSummation' mtx = sumS res
    where applySHY f h sh@(Z :. x :. y) = let x = h $ ix1 y in bool 0 1 (f sh)
          vtrShape = let (Z :. _ :. x) = extent mtx in ix1 x
          vtr = fromFunction vtrShape $ const 0
          res = traverse2 mtx vtr const applySHY

layerSummation :: NNTMU -> NTTVU
layerSummation mtx = sumS $ map (bool 0 1) mtx

layerOperation :: NTTVU -> NTTVU -> (NTT -> NTT -> NNT) -> NNTVU
layerOperation x y f = computeS $ zipWith f x y

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

applyReverseNeuron' :: NNTVU -> CAMNeuron -> NNTVU
applyReverseNeuron' input (CAMNeuron camW camT) = res
    where collision = layerColide (computeS $ transpose camW) input (\x y -> complement $ xor x y)
          summ = layerSummation collision
          res = layerOperation summ camT (<)

applyBackProp :: NNTVU -> NNTVU -> NNTVU -> CAMNeuron -> NNTVU
applyBackProp delta input output (CAMNeuron camW camT) = foldS (.&.) True dWeights
    where dWeights = transpose $ deltaWeights' camW input output delta

vecCompare :: NNTVU -> NNTVU -> NNTVU
vecCompare x y = computeS $ zipWith xor x y

hammingWeight :: NNTVU -> Int
hammingWeight x = sumAllS $ map (bool 0 1) x

trainNeurons :: TrainElem -> CAMUpdate -> [CAMNeuron] -> [CAMNeuron]
trainNeurons (TrainElem train desired) (CAMUpdate lstIndex nnIndex camElem) nn = bool nn' nn (hammingWeight compared == 0)
    where query = foldl applyNeuron train nn
          compared = vecCompare query desired
          chosenNeuron = nn !! lstIndex
          choppedNNR = take (lstIndex + 1) $ reverse nn
          choppedNN = take (lstIndex + 1) nn
          query' = foldl applyReverseNeuron compared choppedNN
          output = foldl applyNeuron train choppedNN
          expected = vecCompare query' output
          camNN = updateCAMNeuron chosenNeuron camElem query' expected nnIndex
          nn' = replaceElem nn lstIndex camNN

trainCAMNN :: [CAMNeuron] -> [CAMUpdate] -> [TrainElem] -> [CAMNeuron]
trainCAMNN nn updates trainSet = foldr (\(x, y) n -> trainNeurons x y n) nn (zip trainSet updates)

queryNeurons :: [CAMNeuron] -> QueryData -> NNTVU
queryNeurons nn query = foldl applyNeuron query nn

queryReverseNeurons :: [CAMNeuron] -> QueryData -> NNTVU
queryReverseNeurons nn query = foldl applyReverseNeuron query nn

queryCAMNN :: [CAMNeuron] -> [TrainElem] -> [NNTVU]
queryCAMNN nn = P.map (\(TrainElem query _) -> queryNeurons nn query)

queryReverseCAMNN :: [CAMNeuron] -> [TrainElem] -> [NNTVU]
queryReverseCAMNN nn = P.map (\(TrainElem query _) -> queryReverseNeurons nn query)

distanceCAMNN :: [CAMNeuron] -> [TrainElem] -> [Int]
distanceCAMNN nn testSet = P.map hammingWeight compared
    where queries = queryCAMNN nn testSet
          compared = P.map (uncurry vecCompare) $ P.zipWith (\x (TrainElem _ y) -> (x,y)) queries testSet

trainUntilLearned :: [CAMNeuron] -> [TrainElem] -> Int -> IO [CAMNeuron]
trainUntilLearned nn trainSet shiftUpdate = do
    let nnLength = length nn
    let updates = constructUpdate nnLength
    let shifted = take (length trainSet) $ applyNTimes updates shiftLeft shiftUpdate
    let nn' = trainCAMNN nn updates trainSet
    -- print nn'
    let distance = sum $ distanceCAMNN nn' trainSet
    bool (trainUntilLearned nn' trainSet (shiftUpdate + 1)) (return nn') (distance < 3)

updateCAMNeuron :: CAMNeuron -> CAMElem -> NNTVU -> NNTVU -> Int -> CAMNeuron
updateCAMNeuron (CAMNeuron camW camT) CAMThreshold delta output elemIndex = CAMNeuron camW threshold
    where threshold = deltaThreshold camT delta output elemIndex
updateCAMNeuron (CAMNeuron camW camT) CAMWeight delta output elemIndex = CAMNeuron weight camT
    where weight = deltaWeights camW delta output elemIndex

deltaThreshold :: NTTVU -> NNTVU -> NNTVU -> Int -> NTTVU
deltaThreshold threshold delta selection elemIndex = computeUnboxedS res
    where deltaAND = zeroButIndex delta elemIndex
          zipped = zipWith (\x y -> bool 0 (bool (-1) 1 y) x) deltaAND selection
          res = zipWith (\x y -> let res = x - y in bool 0 res (res > 0)) threshold zipped

deltaWeights :: NNTMU -> NNTVU -> NNTVU -> Int -> NNTMU
deltaWeights weights delta selection elemIndex = computeUnboxedS res
    where applySHY f g h sh@(Z :. x :. y) = let nSh = ix1 y in xor (f sh) (g nSh) .&. h nSh
          selectionAND = zeroButIndex selection elemIndex
          res = traverse3 weights delta selectionAND (\x _ _ -> x) applySHY

updateCAMNeuron' :: CAMNeuron -> CAMElem -> NNTVU -> NNTVU -> NNTVU -> CAMNeuron
updateCAMNeuron' (CAMNeuron camW camT) CAMThreshold delta output _ = CAMNeuron camW threshold
    where threshold = deltaThreshold' camT delta output
updateCAMNeuron' (CAMNeuron camW camT) CAMWeight delta output input = CAMNeuron weights camT
    where dWeights = deltaWeights' camW input output delta
          weights = applyDelta camW dWeights

deltaThreshold' :: NTTVU -> NNTVU -> NNTVU -> NTTVU
deltaThreshold' threshold delta output = computeUnboxedS res
    where applySHY f g h sh = applyThresholdDelta (f sh) (g sh) (h sh)
          res = traverse3 threshold delta output (\x _ _ -> x) applySHY

deltaWeights' :: NNTMU -> NNTVU -> NNTVU -> NNTVU -> NNTMU
deltaWeights' weights input output delta = computeUnboxedS res
    where applySHY f g h j sh@(Z :. x :. y) = let nSh = ix1 y in applySelection (f sh) (g nSh) (h nSh) (j nSh)
          res = traverse4 weights input output delta (\x _ _ _ -> x) applySHY

applyDelta :: NNTMU -> NNTMU -> NNTMU
applyDelta weights delta = computeUnboxedS $ zipWith applySHY weights delta
    where applySHY x = bool x (complement x)
          atMostOne =

applySelection :: NNT -> NNT -> NNT -> NNT -> NNT
applySelection x y z w = xor (xor x y) (complement z) .&. w

applyThresholdDelta :: NTT -> NNT -> NNT -> NTT
applyThresholdDelta x y z = bool x (bool (x - 1) (x + 1) z) y

zeroButIndex :: NNTVU -> Int -> NNTVU
zeroButIndex selection elemIndex = computeS $ traverse selection id (\f sh@(Z :. x) -> bool False (f sh) (x == elemIndex))

---------------------------------------------------------------------------

replaceElem :: [a] -> Int -> a -> [a]
replaceElem [] _ _ = []
replaceElem x 0 y = y : tail x
replaceElem x n val
    | n >= length x = x
    | otherwise = xs ++ val : ys
        where (xs, _:ys) = splitAt n x

---------------------------------------------------------------------------

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
randomCAMNeuron seed x y = CAMNeuron (randomNNTMU seed x y) (randomNTTVU seed' x)
    where (_, seed') = next seed

-- randomUpdate :: RandomGen g => g -> [CAMNeuron] -> (Int, CAMUpdate)
-- randomUpdate gen nn = (index0, camU)
--     where (updateCAM, gen0) = random gen
--           (index0, gen1) = randomR (0, length nn - 1) gen0
--           chosenNeuron = nn !! index0
--           weightsSh = extent $ camWeights chosenNeuron
--           thresholdSh = extent $ camThresholds chosenNeuron
--           (index1, gen2) = randomR (0, size weightsSh) gen1
--           (index2, gen3) = randomR (0, size thresholdSh) gen2
--           (index3, _) = random gen3
--           typeToUpdate = bool CAMThreshold CAMWeight updateCAM
--           camU = CAMUpdate typeToUpdate (fromIndex weightsSh index1) (fromIndex thresholdSh index2) index3

lstCAMElems :: Int -> [CAMElem]
lstCAMElems elems = [bool CAMWeight CAMThreshold (even x) | x <- [1 .. elems]]

constructUpdate :: Int -> [CAMUpdate]
constructUpdate nnElems = reverse $ concatMap (\(x,y) -> P.map (CAMUpdate x y) camElems) pairLst
    where count = nnElems - 1
          camElems = [CAMWeight, CAMThreshold]
          pairLst = [(x,y) | x <- [0 .. count], y <- [0 .. count]]


---------------------------------------------------------------------------

shiftLeft :: [a] -> [a]
shiftLeft []  = []
shiftLeft [x] = [x]
shiftLeft x   = tail x ++ [head x]

shiftRight :: [a] -> [a]
shiftRight []  = []
shiftRight [x] = [x]
shiftRight x   = last x : init x

applyNTimes :: [a] -> ([a] -> [a]) -> Int -> [a]
applyNTimes x _ 0 = x
applyNTimes x f n = let x' = f x in bool (applyNTimes x' f (n - 1)) x' (n <= 0)

sumTrues :: Num a => Bool -> a -> a
sumTrues cond num = bool num (num + 1) cond

sumBits :: (Bits a, Num a) => [Bool] -> a
sumBits = foldr sumTrues 0

xorPCThreshold :: Bits a => a -> Int -> a -> Bool
xorPCThreshold x y = (<= y) . popCount . xor x

flipElems :: Bits a => a -> a -> a
flipElems = (.|.)

---------------------------------------------------------------------------

trainNeurons' :: TrainElem -> CAMUpdate -> [CAMNeuron] -> [CAMNeuron]
trainNeurons' (TrainElem train desired) (CAMUpdate lstIndex nnIndex camElem) nn = bool nn' nn (hammingWeight compared == 0)
    where query = foldl applyNeuron train nn
          compared = vecCompare query desired
          chosenNeuron = nn !! lstIndex
          choppedNNR = take (lstIndex + 1) $ reverse nn
          choppedNN = take (lstIndex + 1) nn
          hidden = foldl applyNeuron train choppedNN
          output = applyNeuron hidden chosenNeuron
       -- applyBackProp delta input output (CAMNeuron camW camT)
          deltaP = foldl applyReverseNeuron' compared choppedNNR
       -- updateCAMNeuron' (CAMNeuron camW camT) CAMWeight delta output input
          camNN = updateCAMNeuron' chosenNeuron camElem deltaP output hidden
          nn' = replaceElem nn lstIndex camNN

trainCAMNN' :: [CAMNeuron] -> [CAMUpdate] -> [TrainElem] -> [CAMNeuron]
trainCAMNN' nn updates trainSet = foldr (\(x, y) n -> trainNeurons' x y n) nn (zip trainSet updates)
