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

import           CAM                 (distanceCAMNN, trainWithEpochs)
import           CAMRandom           (randomCAMNeuron)
import           CAMTypes            (TrainElem (..))
import qualified Data.Array.Repa     as R
import           Data.Bool           (bool)
import           Data.IDX            (decodeIDXFile, decodeIDXLabelsFile,
                                      labeledIntData)
import           Data.Maybe          (fromJust)
import qualified Data.Vector.Unboxed as V
import           ListExtras          (num2Bin')
import           System.Random       (mkStdGen, split)

runMNIST :: IO ()
runMNIST = do
    print "runMNIST"
    dta <- loadMNISTFiles "../MNIST-Data/t10k-labels-idx1-ubyte" "../MNIST-Data/t10k-images-idx3-ubyte"
    let inputSize = 784
    let outputSize = 4
    let epochs = 5
    let nnSize = 32
    let llSize = 5
    let trainingSet = 8000
    let testingSet = 2000
    let genStd = mkStdGen inputSize
    -- genStd <- newStdGen
    let transformV = R.fromUnboxed (R.ix1 inputSize) . V.map (\z -> bool False True (z >= 50))
    let transformNum = R.fromListUnboxed (R.ix1 outputSize) . reverse . num2Bin' outputSize
    let trainSet = map (\(x,y) -> TrainElem (transformV y) (transformNum x)) $ take trainingSet dta
    let testSet = map (\(x,y) -> TrainElem (transformV y) (transformNum x)) . take nnSize $ drop testingSet dta
    let (genStd0, genStd1) = split genStd
    let camN0 = randomCAMNeuron genStd nnSize inputSize
    let camN1 = randomCAMNeuron genStd0 nnSize nnSize
    let camN2 = randomCAMNeuron genStd0 outputSize nnSize
    let nn = [camN0] ++ replicate llSize camN1 ++ [camN2]
    -- nn' <- trainUntilLearned nn trainSet 0 5
    nn' <- trainWithEpochs nn trainSet 0 epochs
    let distance = distanceCAMNN nn' testSet
    let matches = length . filter (== 0) $ distance
    let percentage = fromIntegral matches / fromIntegral (length distance) * 100
    print $ "Distance: " ++ show distance
    print $ show matches ++ "/" ++ show (length distance) ++ "->" ++ show percentage ++ "%"
    return ()

loadMNISTFiles :: String -> String -> IO [(Int, V.Vector Int)]
loadMNISTFiles labelPath dataPath = do
    maybeLbl <- decodeIDXLabelsFile labelPath
    maybeData <- decodeIDXFile dataPath
    let lbl = fromJust maybeLbl
    let dta = fromJust maybeData
    let mnist = labeledIntData lbl dta
    return $ fromJust mnist

