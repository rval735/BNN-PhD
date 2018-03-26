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
    nnFunction,
    nnFunctionR
)
where

import qualified Data.ByteString.Lazy.Char8           as C8L
import           Data.List                            (intercalate)
import           Data.Maybe                           (mapMaybe)
import           Data.Time.Clock                      (diffUTCTime,
                                                       getCurrentTime)
import           NNClass
import           Numeric.LinearAlgebra.HMatrix        (R, fromList, maxIndex)
import           System.Environment                   (getArgs)
import           System.Exit                          (exitFailure)

import qualified Data.Array.Repa                      as RP
import qualified Data.Array.Repa.Algorithms.Matrix    as RP
import qualified Data.Array.Repa.Algorithms.Randomish as RP


-- Set constant values for input and output nodes
inputNodes :: Int
inputNodes = 784

outputNodes :: Int
outputNodes = 10

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
    nn <- createNN nnBase
    -- Read the contents of the CSV file for training and testing
    trainedNN <- readCVS "../MNIST-Data/MNIST-Train.csv" epochs nn
    testedNN <- queryCVS "../MNIST-Data/MNIST-Test.csv" trainedNN
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
--   elements considering a CSV file format with no header, with
--   the number of epochs, a NN will produce an updated NN
readCVS :: String -> Epochs -> NeuralNetwork -> IO NeuralNetwork
readCVS path epochs nn = analyzeLines epochs nn <$> readCVSFile path

-- | Ask the NN its predictions about a CSV file, then match that
--    with values it is expecting to predict in a list of booleans
queryCVS :: String -> NeuralNetwork -> IO [Bool]
queryCVS path nn = queryLines nn <$> readCVSFile path

-- | From a path to a Lazy ByteString split in lines
readCVSFile :: String -> IO [C8L.ByteString]
readCVSFile path = C8L.lines <$> C8L.readFile path

-- | Iterate a NN by the number of "Epochs" it receives with the
--   line data that is going to be transformed into a represantion the
--   NN is able to understand. It is recursive function until no more
--   "Epochs" are in place
analyzeLines :: Epochs -> NeuralNetwork -> [C8L.ByteString] -> NeuralNetwork
analyzeLines 0 nn _ = nn
analyzeLines epochs nn xs = analyzeLines (epochs - 1) trainedNN xs
    where layers = map cleanLayer xs
          trainedNN = foldr trainNN nn layers

-- | Same principle as "analyzeLines" but just to query the NN
queryLines :: NeuralNetwork -> [C8L.ByteString] -> [Bool]
queryLines _ [] = []
queryLines nn xs = map (queryNN nn) layers
    where layers = map cleanLayer xs

-- | One line function that transform a CSV line to a tuple with
--   (Label, Data) format
cleanLayer :: C8L.ByteString -> (Int, NLayer)
cleanLayer = readDecoded . map fst . mapMaybe C8L.readInt . C8L.split ','

-- | Tuple by tuple, perform the NN tranining from NNClass
trainNN :: (Int, NLayer) -> NeuralNetwork -> NeuralNetwork
trainNN (expected, layer) = train layer (desiredOutput expected)

-- | Tuple by tuple, perform the NN quering from NNClass
queryNN :: NeuralNetwork -> (Int, NLayer) -> Bool
queryNN nn (expected, layer) = matchesIndex (expected, queryLL)
    where queryLL = query nn layer

-- | Transform a list of Int into a tuple with format (Label, Data)
--   for the NN to read
readDecoded :: [Int] -> (Int, NLayer)
readDecoded []     = (0, fromList [])
readDecoded (x:xs) = (x, fromList $ map normalizer xs)

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



---------------------------------------------------------------------------------

nnFunctionR :: IO ()
nnFunctionR = do
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
    trainedNN <- readCVSR "../MNIST-Data/MNIST-Train.csv" epochs nn
    testedNN <- queryCVSR "../MNIST-Data/MNIST-Test.csv" trainedNN
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

readCVSR :: String -> Epochs -> NeuralNetworkR -> IO NeuralNetworkR
readCVSR path epochs nn = readCVSFile path >>= analyzeLinesR epochs nn

analyzeLinesR :: Epochs -> NeuralNetworkR -> [C8L.ByteString] -> IO NeuralNetworkR
analyzeLinesR 0 nn _ = return nn
analyzeLinesR epochs nn xs = do
    let layers = map cleanLayerR xs
    trainedNN <- foldrM trainNNR nn layers
    analyzeLinesR (epochs - 1) trainedNN xs

queryCVSR :: String -> NeuralNetworkR -> IO [Bool]
queryCVSR path nn = readCVSFile path >>= queryLinesR nn

queryLinesR :: NeuralNetworkR -> [C8L.ByteString] -> IO [Bool]
queryLinesR _ [] = return []
queryLinesR nn xs = mapM (queryNNR nn) layers
  where layers = map cleanLayerR xs

trainNNR :: (Int, NLayerR) -> NeuralNetworkR -> IO NeuralNetworkR
trainNNR (expected, layer) = trainR layer (desiredOutputR expected)

cleanLayerR :: C8L.ByteString -> (Int, NLayerR)
cleanLayerR = readDecodedR . map fst . mapMaybe C8L.readInt . C8L.split ','

readDecodedR :: [Int] -> (Int, NLayerR)
readDecodedR []     = (0, xp)
    where xp = RP.fromListUnboxed (RP.ix1 inputNodes) $ replicate inputNodes 0
readDecodedR (x:xs) = (x, xp)
    where xp = RP.fromListUnboxed (RP.ix1 inputNodes) $ map fromIntegral xs

queryNNR :: NeuralNetworkR -> (Int, NLayerR) -> IO Bool
queryNNR nn (expected, layer) = do
    queryLL <- queryR nn layer
    return $ matchesIndexR (expected, queryLL)

matchesIndexR :: (Int, NLayerR) -> Bool
matchesIndexR (index, xs) =  maxVal == valAtIndex
    where maxVal = RP.foldAllS max mostMinDouble xs
          valAtIndex = RP.toList xs !! index

mostMinDouble :: Double
mostMinDouble = fromIntegral (minBound :: Int) :: Double

desiredOutputR :: Int -> NLayerR
desiredOutputR val = RP.fromListUnboxed (RP.ix1 outputNodes) [if x == val then 0.99 else 0.01 | x <- [0 .. 9]]

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM f d = foldr ((=<<) . f) (return d)
