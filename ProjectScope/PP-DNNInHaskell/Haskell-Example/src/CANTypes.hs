----
---- CNN-PhD version 0.1, Copyright (C) 24/Jul/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | File that keeps all types and CAN definitions together
module CANTypes
-- (
-- )
where


import           Data.Array.Repa                   (Any, Array, D, DIM1, DIM2,
                                                    Shape, U, Z, extent,
                                                    listOfShape, toList)
import           Data.Array.Repa.Algorithms.Matrix (col, row)
import           Data.Bool                         (bool)
import           ListExtras                        (splitEvery)

type WeightsSize = Int
type ThresholdSize = Int

initialValue :: Int
initialValue = -1

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
type NTTVD = Array D VShape NTT
type NTTVF r = Array r VShape NTT

data CANNeuron = CANNeuron {
    canWeights    :: CANWElem,
    canThresholds :: CANTElem
} deriving (Eq)

data CANNeuronD = CANNeuronD {
    canWeightsD    :: CANWElemD,
    canThresholdsD :: CANTElemD
} deriving (Eq)

instance Show CANNeuron where
    show = neuronString

data CANWElem = CANWElem {
    wChange  :: Int,
    canWElem :: NNTMU
} deriving (Eq)

data CANWElemD = CANWElemD {
    wChangeD  :: Int,
    canWElemD :: NNTMD
} deriving (Eq)

instance Show CANWElem where
    show = weightsString

data CANTElem = CANTElem {
    tChange  :: Int,
    canTElem :: NTTVU
} deriving (Eq)

data CANTElemD = CANTElemD {
    tChangeD  :: Int,
    canTElemD :: NTTVD
} deriving (Eq)

instance Show CANTElem where
    show (CANTElem tC cT) = show tC ++ ":" ++ show (toList cT)

type CANNN = [CANNeuron]

data CANElem = CANWeight | CANThreshold
    deriving (Eq, Show)

data CANUpdate = CANUpdate {
    lstIndex :: Int,
    canElem  :: CANElem
}

instance Show CANUpdate where
    show (CANUpdate lI cE) = "{" ++ show lI ++ ":" ++ show cE ++ "}"

data TrainElem = TrainElem {
    trainInput  :: NNTVU,
    trainOutput :: NNTVU
}

instance Show TrainElem where
    show (TrainElem tI tO) = "I:" ++ show (toBList tI) ++ "\nO:" ++ show (toBList tO)

---------------------------------------
------------- Helper List -------------
---------------------------------------

toBList :: (Shape sh) => Array U sh Bool -> [Int]
toBList = map (bool 0 1) . toList

neuronString :: CANNeuron -> String
neuronString (CANNeuron (CANWElem wC cW) (CANTElem tC cT)) = changesStr ++ formatted
    where lstW = toBList cW
          lstT = toList cT
          colW = col $ extent cW
          splitted = zip (splitEvery colW lstW) lstT
          formatted = foldl (\z (x, y) -> z ++ "\n" ++ show x ++ ":" ++ show y) "" splitted
          changesStr =  "(" ++ show wC ++ "," ++ show tC ++ ")"

weightsString :: CANWElem -> String
weightsString (CANWElem wC cW) = show wC ++ formatted
    where lstW = toBList cW
          colW = col $ extent cW
          splitted = splitEvery colW lstW
          formatted = foldl (\y x -> y ++ "\n" ++ show x) "" splitted

printNN :: [CANNeuron] -> [CANNeuron] -> IO ()
printNN nn nn' = do
    let zipped = zip nn nn'
    print "---------------"
    mapM_ (\(x,y) -> do print "NN:"; print x; print "NN':"; print y ) zipped
