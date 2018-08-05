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

runMNIST :: IO ()
runMNIST = do
    dta <- loadMNISTFiles "../MNIST-Data/t10k-labels-idx1-ubyte" "../MNIST-Data/t10k-images-idx3-ubyte"
    let outputSize = 4
    let nnSize = 16
    let inputSize = 784
    let llSize = 3
    let transformV = R.fromUnboxed (R.ix1 inputSize) . V.map (\z -> bool False True (z >= 50))
    let transformNum = R.fromListUnboxed (R.ix1 outputSize) . reverse . num2Bin' outputSize
    let trainSet = map (\(x,y) -> TrainElem (transformV y) (transformNum x)) $ take nnSize dta
    let camN0 = createZNeuron nnSize inputSize
    let camN1 = createZNeuron nnSize nnSize
    let camN2 = createZNeuron outputSize nnSize
    let nn = [camN0] ++ replicate llSize camN1 ++ [camN2]
    print "To Train"
    nn' <- trainUntilLearned nn trainSet 0 5


    return ()

loadMNISTFiles :: String -> String -> IO [(Int, V.Vector Int)]
loadMNISTFiles labelPath dataPath = do
    maybeLbl <- decodeIDXLabelsFile labelPath
    maybeData <- decodeIDXFile dataPath
    let lbl = fromJust maybeLbl
    let dta = fromJust maybeData
    let mnist = labeledIntData lbl dta
    return $ fromJust mnist

