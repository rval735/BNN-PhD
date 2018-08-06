----
---- CNN-PhD version 0.1, Copyright (C) 5/Aug/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Test CAM for the MNIST dataset
module CAMMNISTTests
-- (
-- )
where

import           CAM
import           CAMExtras
import           CAMTypes
import           Control.Arrow       (second)
import           Control.Monad       (when)
import qualified Data.Array.Repa     as R
import           Data.Bits           (xor)
import           Data.Bool           (bool)
import           Data.IDX            (decodeIDXFile, decodeIDXLabelsFile,
                                      labeledIntData)
import           Data.List           (zip5)
import           Data.Maybe          (fromJust)
import qualified Data.Vector.Unboxed as V
import           ListExtras          (applyNTimes, indexFirstNonZeroI, num2Bin',
                                      replaceElem, safeHead, shiftLeft)
import           System.Random       (mkStdGen, newStdGen, next)

runMNIST :: IO ()
runMNIST = do
    print "runMNIST"
    dta <- loadMNISTFiles "../MNIST-Data/t10k-labels-idx1-ubyte" "../MNIST-Data/t10k-images-idx3-ubyte"
    let outputSize = 4
    let nnSize = 16
    let inputSize = 784
    let llSize = 5
    let epochs = 10
    let trainingSet = 1000
    let testingSet = 200
    -- let genStd = mkStdGen inputSize
    genStd <- newStdGen
    let transformV = R.fromUnboxed (R.ix1 inputSize) . V.map (\z -> bool False True (z >= 50))
    let transformNum = R.fromListUnboxed (R.ix1 outputSize) . reverse . num2Bin' outputSize
    let trainSet = map (\(x,y) -> TrainElem (transformV y) (transformNum x)) $ take trainingSet dta
    let testSet = map (\(x,y) -> TrainElem (transformV y) (transformNum x)) . take nnSize $ drop testingSet dta
    let (seed0, genStd0) = next genStd
    let (seed1, genStd1) = next genStd0
    let (seed2, genStd2) = next genStd1
    let camN0 = createRNeuron nnSize inputSize seed0
    let camN1 = createRNeuron nnSize nnSize seed1
    let camN2 = createRNeuron outputSize nnSize seed2
    let nn = [camN0] ++ replicate llSize camN1 ++ [camN2]
    -- nn' <- trainUntilLearned nn trainSet 0 5
    nn' <- trainWithEpochs nn trainSet 0 epochs
    let distance = distanceCAMNN nn' testSet
    let matches = fromIntegral . length . filter (== 0) $ distance
    let percentage = matches / fromIntegral (length distance) * 100
    print $ "Distance: " ++ show distance
    print $ "Percentage: " ++ show percentage
    return ()

loadMNISTFiles :: String -> String -> IO [(Int, V.Vector Int)]
loadMNISTFiles labelPath dataPath = do
    maybeLbl <- decodeIDXLabelsFile labelPath
    maybeData <- decodeIDXFile dataPath
    let lbl = fromJust maybeLbl
    let dta = fromJust maybeData
    let mnist = labeledIntData lbl dta
    return $ fromJust mnist

