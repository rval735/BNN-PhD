----
---- CNN-PhD version 0.1, Copyright (C) 24/Jul/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | File that keeps all types and CAM definitions together
module CAMTypes
-- (
-- )
where


import           Data.Array.Repa (Array, D, DIM1, DIM2, Shape, U, toList)
import           Data.Bool       (bool)

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
    camWeights    :: CAMWElem,
    camThresholds :: CAMTElem
} deriving (Eq)

instance Show CAMNeuron where
    show (CAMNeuron cW cT) = show cW ++ "\n " ++ show cT ++ "\n"

data CAMWElem = CAMWElem {
    camWElem :: NNTMU,
    wChange  :: Int
} deriving (Eq)

instance Show CAMWElem where
    show (CAMWElem cW wC) = show wC ++ ":" ++ show (toBList cW)

data CAMTElem = CAMTElem {
    camTElem :: NTTVU,
    tChange  :: Int
} deriving (Eq)

instance Show CAMTElem where
    show (CAMTElem cT tC) = show tC ++ ":" ++ show (toList cT)

type CAMNN = [CAMNeuron]

data CAMElem = CAMWeight | CAMThreshold
    deriving (Eq, Show)

data CAMUpdate = CAMUpdate {
    lstIndex :: Int,
    camElem  :: CAMElem
}

instance Show CAMUpdate where
    show (CAMUpdate lI cE) = "{" ++ show lI ++ ":" ++ show cE ++ "}"

data TrainElem = TrainElem {
    trainInput  :: NNTVU,
    trainOutput :: NNTVU
}

instance Show TrainElem where
    show (TrainElem tI tO) = show (toBList tI) ++ "\n " ++ show (toBList tO) ++ "\n"

type QueryData = NNTVU


---------------------------------------
------------- Helper List -------------
---------------------------------------

toBList :: (Shape sh) => Array U sh Bool -> [Int]
toBList = map (bool 0 1) . toList
