----
---- CNN-PhD version 0.1, Copyright (C) 5/Mar/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----
-- module Main where

{-# LANGUAGE BangPatterns #-}

module Lib
-- (
--     nnFunction,
--     readElems,
-- )
where

import qualified Data.ByteString.Lazy.Char8    as C8L
-- import qualified Data.ByteString.Lazy          as BSL
import           Data.Csv
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
import qualified Data.Vector                   as DV
import           NNClass
import           Numeric.LinearAlgebra.Devel
import           System.Environment
import           System.Exit
                                            -- (mapMatrixWithIndex,
                                            --  mapVectorWithIndex)
import           Numeric.LinearAlgebra.HMatrix

type DVector = DV.Vector

nnFunction :: IO ()
nnFunction = do
    startTime <- getCurrentTime
    elems <- getArgs
    -- if length elems < 3 then
    --     exitFailure
    -- else

    let inputNodes = 784
    let outputNodes = 10
    let (nnBase, epochs) = readElems elems inputNodes outputNodes
    nn <- createNN nnBase

    trainedNN <- {-# SCC "readCVS" #-} readCVS "../MNIST-Data/MNIST-Train.csv" epochs nn
    testedNN <- {-# SCC "readCVS" #-} queryCVS "../MNIST-Data/MNIST-Test.csv" trainedNN

    -- let updatedNN = {-# SCC "training" #-} doTraining epochs nn trainingDF
    -- let matches = {-# SCC "query" #-} queryNN updatedNN testDF
    let numberOfMatches = {-# SCC "matches" #-} foldr (\x y -> if x then y + 1 else y) 0 testedNN
    let !matchesError = {-# SCC "matchesError" #-} fromIntegral numberOfMatches / (fromIntegral . length $ testedNN)

    endTime <- getCurrentTime

    let diff = diffUTCTime endTime startTime
    print "HNodes, LRate, Epochs, Error, Diff, STime, ETime"
    let elems = [show (hnodes nnBase), show (baseLRate nnBase), show epochs, {-show matchesError ,-} show diff, show startTime, show endTime]
    print $ intercalate ", " elems


readElems :: [String] -> InputNodes -> OutputNodes -> (NNBase, Epochs)
readElems [] iN oN = (NNBase iN 1 oN 0.01, 0)
readElems [x, y, z] iN oN = (NNBase iN xx oN yy, zz)
    where xx = read x :: HiddenNodes
          yy = read y :: LearningRate
          zz = read z :: Epochs
readElems _ iN oN =  (NNBase iN 1 oN 0.01, 0)

readCVS :: String -> Epochs -> NeuralNetwork -> IO NeuralNetwork
readCVS path epochs nn = analyzeLines epochs nn  <$> readCVSFile path
        -- let enlined = decode NoHeader csvData
        -- return $ readDecoded enlined
    -- let enlined = map (map fst . mapMaybe C8.readInt . C8.split ',') . C8.lines $ cvsData
    -- let (elems, rep) = unzip $ map (splitAt 1) enlined
    -- let simplified = map head elems
    -- let floated = map (fromList . map normalizer) rep
    -- return $ zip simplified floated

queryCVS :: String -> NeuralNetwork -> IO [Bool]
queryCVS path nn = queryLines nn  <$> readCVSFile path

readCVSFile :: String -> IO [C8L.ByteString]
readCVSFile path = C8L.lines <$> C8L.readFile path

analyzeLines :: Epochs -> NeuralNetwork -> [C8L.ByteString] -> NeuralNetwork
analyzeLines 0 nn _ = nn
analyzeLines epochs nn xs = analyzeLines (epochs - 1) trainedNN xs
    where layers = map cleanLayer xs
          trainedNN = foldr trainNN nn layers

queryLines :: NeuralNetwork -> [C8L.ByteString] -> [Bool]
queryLines _ [] = []
queryLines nn xs = map (queryNN' nn) layers
    where layers = map cleanLayer xs

cleanLayer :: C8L.ByteString -> (Int, NLayer)
cleanLayer = readDecoded . map fst . mapMaybe C8L.readInt . C8L.split ','

trainNN :: (Int, NLayer) -> NeuralNetwork -> NeuralNetwork
trainNN (expected, layer) = train layer (desiredOutput expected)

queryNN' :: NeuralNetwork -> (Int, NLayer) -> Bool
queryNN' nn (expected, layer) = matchesIndex (expected, queryLL)
    where queryLL = query nn layer

readDecoded :: [Int] -> (Int, NLayer)
readDecoded []     = (0, fromList [])
readDecoded (x:xs) = (x, fromList $ map fromIntegral xs)

transformTuple :: (DVector R, DVector R) -> (Int, NLayer)
transformTuple (x, y)  = (round $ DV.head x, vectorized)
    where partial = DV.map normalizer y
          vectorized = fromList $ DV.toList partial

normalizer :: R -> R
normalizer x = 0.01 + x / 255 * 0.99

desiredOutput :: Int -> NLayer
desiredOutput val = fromList [if x == val then 0.99 else 0.01 | x <- [0 .. 9]]

doTraining :: Epochs -> NeuralNetwork -> DVector (Int, NLayer) -> NeuralNetwork
doTraining 0 nn _ = nn
doTraining x nn trainData = doTraining (x - 1) iterNN trainData
    where iterNN = DV.foldr (\(x,y) -> train y (desiredOutput x)) nn trainData

queryNN :: NeuralNetwork -> DVector (Int, NLayer) -> DVector Bool
queryNN nn testData = results
    where (expected, test) = DV.unzip testData
          queried = DV.map (query nn) test
          dualQuery = DV.zip expected queried
          results = DV.map matchesIndex dualQuery

matchesIndex :: (Int, NLayer) -> Bool
matchesIndex (index, xs) = maxIndex xs == index
