----
---- CNN-PhD version 0.1, Copyright (C) 5/Mar/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE BangPatterns #-}

module Lib
(
    nnFunction
)
where

import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.List                  (intercalate)
import           Data.Maybe                 (mapMaybe)
import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           NNClass
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)

import           Data.Array.Repa            hiding (map)
import           Data.Foldable              (foldrM)

-- Set constant values for input and output nodes
inputNodes :: Int
inputNodes = 784

outputNodes :: Int
outputNodes = 10

data MImg = MImg {
    valueInt :: Int,
    valueVec :: NLayerU,
    dataVec  :: NLayerU
} deriving (Show)

-- | Main NN function that is part of IO, here data loading, training
--   and quering will take place.
nnFunction :: IO ()
nnFunction = do
    -- Record start time
    startTime <- getCurrentTime
    -- Read command line arguments passed to the program
    elems <- getArgs
    -- Confirm that the elements passed as program arguments
    -- match the expected values.
    (nnBase, epochs) <- readElems elems inputNodes outputNodes
    -- Create a NN with the values read from arguments
    nn <- createNNR nnBase

    -- Read the contents of the CSV file for training and testing
    !trainCSV <- readCSVFile "../MNIST-Data/MNIST-Train.csv"
    !testCSV <- readCSVFile "../MNIST-Data/MNIST-Test.csv"

    -- With the contents read, train the NN
    trainedNN <- analyzeLines epochs nn trainCSV
    testedNN <- queryLines trainedNN testCSV

    -- Compare the results with the number of elements were passed
    let numberOfMatches = foldr (\x y -> if x then y + 1 else y) 0 testedNN
    -- Make the strict call to calculate all previous lines, making a ratio
    -- of the number of correct values against the elements that we tested
    let !matchesError = fromIntegral numberOfMatches / (fromIntegral . length $ testedNN)
    -- Get the time of executing the whole venture
    endTime <- getCurrentTime
    -- Take the difference of that time
    let diff = diffUTCTime endTime startTime
    -- Print the results
    print "HNodes, LRate, Epochs, Error, Diff, STime, ETime"
    let elems = [show (hnodes nnBase), show (baseLRate nnBase), show epochs, show matchesError , show diff, show startTime, show endTime]
    print $ intercalate ", " elems

-- | From a list of strings, match the elements that are expected from the
--   user input. Consider that it is not error safe and any unexpected value
--   will result in a run time failure
readElems :: [String] -> InputNodes -> OutputNodes -> IO (NNBase, Epochs)
readElems [] _ _ = exitFailure
readElems [x, y, z] iN oN = return (NNBase iN xx oN yy, zz)
    where xx = read x :: HiddenNodes
          yy = read y :: LearningRate
          zz = read z :: Epochs
readElems _ _ _ =  exitFailure

-- | From a path, read the contents of a file, then parse those
--   elements considering a CSV file format with no header
--   to a MImg using Lazy ByteString in the middle.
readCSVFile :: String -> IO [MImg]
readCSVFile path = map readMImg . C8L.lines <$> C8L.readFile path

-- | Iterate a NN by the number of "Epochs" it receives with the
--   line data that is going to be transformed into a represantion the
--   NN is able to understand. It is recursive function until no more
--   "Epochs" are in place
analyzeLines :: (Monad m) => Epochs -> NeuralNetwork -> [MImg] -> m NeuralNetwork
analyzeLines 0 nn _ = return nn
analyzeLines epochs nn xs = do
    trainedNN <- {-# SCC "trainedNN" #-} foldrM trainNN nn xs
    analyzeLines (epochs - 1) trainedNN xs

-- | Same principle as "analyzeLines" but just to query the NN
queryLines :: NeuralNetwork -> [MImg] -> IO [Bool]
queryLines _ []  = return []
queryLines nn xs = mapM (queryNN nn) xs

-- | Tuple by tuple, perform the NN tranining from NNClass
trainNN :: (Monad m) => MImg -> NeuralNetwork -> m NeuralNetwork
trainNN (MImg _ expected layer) = {-# SCC "trainNN" #-} train layer expected

-- | Tuple by tuple, perform the NN quering from NNClass
queryNN :: (Monad m) => NeuralNetwork -> MImg -> m Bool
queryNN nn (MImg vI vV dV) = matchesIndex vI <$> query nn dV

-- | One line function that transform a CSV line to a MImg container
readMImg :: C8L.ByteString -> MImg
readMImg = decodeMImg . map fst . mapMaybe C8L.readInt . C8L.split ','

-- | Transform a list of Int into a MImg container. cData means
--   container data and dData is desired outcome
decodeMImg :: [Int] -> MImg
decodeMImg []     = MImg 0 dData cData
    where dData = desiredOutput 0
          cData = fromListUnboxed (ix1 inputNodes) $ replicate inputNodes 0
decodeMImg (x:xs) = MImg x cData dData
    where cData = desiredOutput x
          dData = fromListUnboxed (ix1 inputNodes) $ map normalizer xs

-------------------------------------------
------------- Simple Functions ------------
-------------------------------------------
-- | Normalize a value that is between 0 and 255 so it becomes 0.01 - 0.99
normalizer :: Int -> NNT
normalizer x = 0.01 + fromIntegral x / 255 * 0.99

-- | Consider if the Layer prediction matches the index in which the
--   expected value should be. For example, parameters like:
--   3, [0,0,1,5,0,0]
--   would return "True", because "5" is the max value and it is
--   placed in the index "3" of the array.
matchesIndex :: Int -> NLayerU -> Bool
matchesIndex index xs = maxVal == valAtIndex
    where maxVal = foldAllS max mostMinDouble xs
          valAtIndex = toList xs !! index

-- | Create a layer of 10 elements that has a maximum value of 0.99 in the
--   "val" position, otherwise 0.01
desiredOutput :: Int -> NLayerU
desiredOutput val = fromListUnboxed (ix1 outputNodes) [if x == val then 0.99 else 0.01 | x <- [0 .. 9]]

-- | Use the integer minimum bound as a measure for Double types
mostMinDouble :: Double
mostMinDouble = fromIntegral (minBound :: Int) :: Double
