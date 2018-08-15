----
---- CNN-PhD version 0.1, Copyright (C) 5/Aug/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE BangPatterns #-}

-- | Test CAN for the MNIST dataset
module CANMNIST
-- (
--     runMNIST
-- )
where

import           CAN                 (distanceCANNN, trainWithEpochs)
import           CANRandom           (randomCANNeuron)
import           CANTypes            (TrainElem (..))
import qualified Data.Array.Repa     as R
import           Data.Bool           (bool)
import           Data.IDX            (decodeIDXFile, decodeIDXLabelsFile,
                                      labeledIntData)
import           Data.List           (intercalate)
import           Data.Maybe          (fromJust)
import           Data.Time.Clock     (diffUTCTime, getCurrentTime)
import qualified Data.Vector.Unboxed as V
import           ListExtras          (num2Bin')
import           System.Random       (mkStdGen, split)

runMNIST :: IO ()
runMNIST = do
    print "runMNIST"
    -- Record start time
    startTime <- getCurrentTime
    dta <- loadMNISTFiles "../MNIST-Data/t10k-labels-idx1-ubyte" "../MNIST-Data/t10k-images-idx3-ubyte"
    let inputSize = 784
    let outputSize = 4
    let epochs = 5
    let nnSize = 16
    let llSize = 2
    let trainingSet = 800
    let testingSet = 200
    let genStd = mkStdGen inputSize
    -- genStd <- newStdGen
    let transformV = R.fromUnboxed (R.ix1 inputSize) . V.map (\z -> bool False True (z >= 50))
    let transformNum = R.fromListUnboxed (R.ix1 outputSize) . reverse . num2Bin' outputSize
    let trainSet = map (\(x,y) -> TrainElem (transformV y) (transformNum x)) $ take trainingSet dta
    let testSet = map (\(x,y) -> TrainElem (transformV y) (transformNum x)) . take testingSet $ drop trainingSet dta
    let (genStd0, genStd1) = split genStd
    let canN0 = randomCANNeuron genStd nnSize inputSize
    let canN1 = randomCANNeuron genStd0 nnSize nnSize
    let canN2 = randomCANNeuron genStd0 outputSize nnSize
    let nn = [canN0] ++ replicate llSize canN1 ++ [canN2]
    -- nn' <- trainUntilLearned nn trainSet 0 5
    let !nnM = trainWithEpochs nn trainSet 0 epochs
    nn' <- nnM
    let distance = distanceCANNN nn' testSet
    let matches = length . filter (== 0) $ distance
    let percentage = fromIntegral matches / fromIntegral (length distance) * 100
    -- Get the time of executing the whole venture
    print $ "Matches: " ++ show matches
    endTime <- getCurrentTime
    -- Take the difference of that time
    let diff = diffUTCTime endTime startTime
    print $ "Samples: " ++ show (length distance)
    print $ "Error: " ++ show percentage ++ "%"
    print $ "HNodes: " ++ show llSize
    print $ "Epochs: " ++  show epochs
    print $ "Start Time: " ++ show startTime
    print $ "End Time: " ++ show endTime
    print $ "Diff: " ++ show diff
    return ()

loadMNISTFiles :: String -> String -> IO [(Int, V.Vector Int)]
loadMNISTFiles labelPath dataPath = do
    maybeLbl <- decodeIDXLabelsFile labelPath
    maybeData <- decodeIDXFile dataPath
    let lbl = fromJust maybeLbl
    let dta = fromJust maybeData
    let mnist = labeledIntData lbl dta
    return $ fromJust mnist
