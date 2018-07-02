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
    camWeigths    :: NNTMU,
    camThresholds :: NTTVU
} deriving (Eq, Show)

type CAMNN = [CAMNeuron]

data CAMElem = CAMWeigth | CAMThreshold
    deriving (Eq, Show)

data CAMUpdate = CAMUpdate CAMElem WeightsSize ThresholdSize
    deriving (Show)

data CAMUpdate' = CAMUpdate' {
        camElem :: CAMElem,
        shapeW  :: DIM2,
        shapeT  :: DIM1
    }
    deriving (Show)


updateCAMNeuron :: CAMNeuron -> CAMUpdate -> CAMNeuron
updateCAMNeuron (CAMNeuron camW camT) (CAMUpdate camElem wIndex tIndex)
    | camElem == CAMThreshold = CAMNeuron camW reduceElem
    | otherwise = CAMNeuron flipElem camT
        where shapeT = ix1 tIndex
              shapeW = ix2 wIndex tIndex
              reduceElem = transformElem camT shapeT (\x -> bool (x - 1) 0 (x <= 0))
              flipElem = transformElem camW shapeW complement

transformElem :: (Shape sh, Bits a, Unbox a) => Array U sh a -> sh -> (a -> a)-> Array U sh a
transformElem mtx sh trans = computeUnboxedS res
    where res = traverse mtx id (\f shape -> bool (f shape) (trans $ f shape) (shape == sh))

layerColide :: NNTMU -> NNTVU -> (NNT -> NNT -> NNT)-> NNTMU
layerColide mtx vtr colision = computeUnboxedS $ traverse2 mtx vtr const applySHY
    where applySHY f h sh@(Z :. x :. y) = colision (f sh) (h (ix1 y))

layerSummation' :: NNTMU -> NTTVU
layerSummation' mtx = sumS res
    where applySHY f h sh@(Z :. x :. y) = let x = h (Z :. y) in bool 0 1 (f sh)
          vtrShape = let (Z :. _ :. x) = extent mtx in ix1 x
          vtr = fromFunction vtrShape $ const 0
          res = traverse2 mtx vtr const applySHY

layerSummation :: NNTMU -> NTTVU
layerSummation mtx = sumS $ map (bool 0 1) mtx

layerComparison :: NTTVU -> NTTVU -> NNTVU
layerComparison x y = computeS $ zipWith (>=) x y

applyNeuron :: NNTVU -> CAMNeuron -> NNTVU
applyNeuron input (CAMNeuron camW camT) = res
    where collision = layerColide camW input xor
          summ = layerSummation collision
          res = layerComparison summ camT

vecCompare :: NNTVU -> NNTVU -> NNTVU
vecCompare x y = computeS $ zipWith xor x y

hammingDistance :: NNTVU -> Int
hammingDistance x = sumAllS $ map (bool 0 1) x

trainCAMNN :: NNTVU -> NNTVU -> (Int, CAMUpdate) -> [CAMNeuron] -> [CAMNeuron]
trainCAMNN train desired (index, update) nn = bool nn' nn (hammingDistance compared == 0)
    where query = foldl applyNeuron train nn
          compared = vecCompare query desired
          chosenNeuron = nn !! index
          camNN = updateCAMNeuron chosenNeuron update
          nn' = replaceElem nn index camNN

generateUpdate :: RandomGen g => g -> [CAMNeuron] -> (Int, CAMUpdate')
generateUpdate gen nn = (index0, camU)
    where (updateCAM, gen0) = random gen
          (index0, gen1) = randomR (0, length nn - 1) gen0
          chosenNeuron = nn !! index0
          weigthsSh = extent $ camWeigths chosenNeuron
          thresholdSh = extent $ camThresholds chosenNeuron
          (index1, gen2) = randomR (0, size weigthsSh) gen1
          (index2, gen3) = randomR (0, size thresholdSh) gen2
          typeToUpdate = bool CAMThreshold CAMWeigth updateCAM
          camU = CAMUpdate' typeToUpdate (fromIndex weigthsSh index1) (fromIndex thresholdSh index2)-- toUpdateIndex1 toUpdateIndex2

-- trainCAMNN :: RandomGen g => g -> NNTVU -> NNTVU -> [CAMNeuron] -> [CAMNeuron]
-- trainCAMNN gen train desired nn = bool nn' nn (hammingDistance compared == 0)
--     where query = foldl applyNeuron train nn
--           compared = vecCompare query desired
--           (toUpdateCAM, gen0) = random gen
--           (toUpdateIndex0, gen1) = randomR (1, length nn) gen0
--           chosenNeuron = nn !! toUpdateIndex0
--           -------------------
--           (toUpdateIndex1, gen2) = randomR (0, size . extent $ camWeigths chosenNeuron) gen1
--           -------------------
--           (toUpdateIndex2, gen3) = randomR (0, size . extent $ camThresholds chosenNeuron) gen2
--           typeToUpdate = bool CAMThreshold CAMWeigth toUpdateCAM
--           camU = CAMUpdate typeToUpdate toUpdateIndex1 toUpdateIndex2
--           camNN = updateCAMNeuron chosenNeuron camU
--           nn' = replaceElem nn toUpdateIndex0 camNN


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

---------------------------------------------------------------------------

sumTrues :: Num a => Bool -> a -> a
sumTrues cond num = bool num (num + 1) cond

sumBits :: (Bits a, Num a) => [Bool] -> a
sumBits = foldr sumTrues 0

xorPCThreshold :: Bits a => a -> Int -> a -> Bool
xorPCThreshold x y = (<= y) . popCount . xor x

flipElems :: Bits a => a -> a -> a
flipElems = (.|.)

algoCAM :: [(Word32, Word32)] -> [[Word32]] -> [[Word32]]
algoCAM elems toTrain = undefined


