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

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8
import           Data.List                     (intercalate)
import           Data.Maybe                    (mapMaybe)
import           Data.Time.Clock               (diffUTCTime, getCurrentTime)
import           NNClass
import           Numeric.LinearAlgebra.HMatrix (R, fromList, maxIndex)
import           System.Environment            (getArgs)
import           System.Exit                   (exitFailure)

-- | Main NN function that is part of IO, here data loading, training
--   and quering will take place.
nnFunction :: IO ()
nnFunction = do
    -- Record start time
    startTime <- getCurrentTime
    -- Read command line arguments passed to the program
    elems <- getArgs
    -- Set constant values for input and output nodes
    let inputNodes = 784
    let outputNodes = 10
    -- Confirm that the elements passed as program arguments
    -- match the expected values.
    (nnBase, epochs) <- readElems elems inputNodes outputNodes
    -- Create a NN with the values read from arguments
    nn <- createNN nnBase
    -- Read the contents of the CSV file for training and testing
    !trainingDF <- readCVS "../MNIST-Data/MNIST-Train.csv"
    !testDF <- readCVS "../MNIST-Data/MNIST-Test.csv"
    -- Peform the training of the NN
    let updatedNN = doTraining epochs nn trainingDF
    -- Query the updated NN and make a list of matches
    let matches = queryNN updatedNN testDF
    -- Compare the results with the number of elements were passed
    let numberOfMatches = foldr (\x y -> if x then y + 1 else y) 0 matches
    -- Make the strict call to calculate all previous lines, making a ratio
    -- of the number of correct values against the elements that we tested
    let !matchesError = fromIntegral numberOfMatches / (fromIntegral . length $ matches)
    -- Get the time of executing the whole venture
    endTime <- getCurrentTime
    -- Take the difference of that time
    let diff = diffUTCTime endTime startTime
    -- Print the results
    print "HNodes, LRate, Epochs, Error, Diff, STime, ETime"
    let elems = [show (hnodes nnBase), show (baseLRate nnBase), show epochs, show matchesError, show diff, show startTime, show endTime]
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
readElems _ iN oN = exitFailure

-- | From a path, read the contents of a file, then parse those
--   elements considering a CSV file format with no header.
readCVS :: String -> IO [(Int, NLayer)]
readCVS path = do
    cvsData <- BS.readFile path
    let enlined = map (map fst . mapMaybe C8.readInt . C8.split ',') . C8.lines $ cvsData
    let (elems, rep) = unzip $ map (splitAt 1) enlined
    let simplified = map head elems
    let floated = map (fromList . map normalizer) rep
    return $ zip simplified floated

-- | Iterate a NN by the number of "Epochs" it receives with the
--   tuple data that represents the "Expected Value" and layer "data" to
--   feed the NN. It is recursive function until no more "Epochs" are in place
doTraining :: Epochs -> NeuralNetwork -> [(Int, NLayer)] -> NeuralNetwork
doTraining 0 nn _ = nn
doTraining x nn trainData = doTraining (x - 1) iterNN trainData
    where iterNN = foldlSeq (\nNN (x,y) -> train y (desiredOutput x) nNN) nn trainData

-- | Ask the NN its predictions about a layer, then match that with the value
--   it is expected to predict.
queryNN :: NeuralNetwork -> [(Int, NLayer)] -> [Bool]
queryNN nn testData = results
    where (expected, test) = unzip testData
          queried = map (query nn) test
          dualQuery = zip expected queried
          results = map matchesIndex dualQuery

---------------- Simple Functions

-- | Normalize a value that is between 0 and 255 so it becomes 0.01 - 0.99
normalizer :: Int -> R
normalizer x = 0.01 + fromIntegral x / 255 * 0.99

-- | Consider if the Layer prediction matches the index in which the
--   expected value should be. For example, a tuple of:
--   (3, [0,0,1,5,0,0])
--   would return "True", because "5" is the max value and it is
--   placed in the index "3" of the array.
matchesIndex :: (Int, NLayer) -> Bool
matchesIndex (index, xs) = maxIndex xs == index

-- | Create a layer of 10 elements that has a maximum value of 0.99 in the
--   "val" position, otherwise 0.01
desiredOutput :: Int -> NLayer
desiredOutput val = fromList [if x == val then 0.99 else 0.01 | x <- [0 .. 9]]

-- | Strict version of foldl, which means it evaluates at the moment of
--   being called instead of lazily
foldlSeq :: (t -> a -> t) -> t -> [a] -> t
foldlSeq f z []     = z
foldlSeq f z (x:xs) = let z' = z `f` x in seq z' $ foldlSeq f z' xs
