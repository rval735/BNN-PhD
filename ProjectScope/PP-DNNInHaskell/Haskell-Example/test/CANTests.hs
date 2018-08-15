----
---- CNN-PhD version 0.1, Copyright (C) 1/Aug/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Unit test for some critical functions of the CAN file
module CANTests
-- (
-- )
where

import           CAN
import           CANExtras
import           CANTypes
import           Control.Monad   (when)
import qualified Data.Array.Repa as R
import           Data.Bits       (xor)
import           Data.Bool       (bool)
import           Data.List       (zip5)
import           ListExtras      (applyNTimes, shiftLeft)

layerColideTest :: IO ()
layerColideTest = do
    print "layerColideTest"
    --- To be tested
    let result = map (\(r, c, x, y, z) -> layerColideOne (r, c) x y z) testExamples
    --- Finish testing
    printResult result

layerSummationTest :: IO ()
layerSummationTest = do
    print "layerSummationTest"
    let weights = map (\(x, y, _, _, z) -> createWeight x y z) testExamples
    let expected = map (\x -> createThreshold (length x) 0 x) [[1,2], [0,0,2], [0,2,0], [1,3], [1,1], [0,0]]
    --- To be tested
    let result = zipWith (==) expected $ map layerSummation weights
    --- Finish testing
    printResult result

trainCANNNTest_2x2NNXOR :: IO ()
trainCANNNTest_2x2NNXOR = do
    print "trainCANNNTest_2x2NNXOR"
    let nSize = 2
    let nn = replicate nSize $ createZNeuron nSize nSize
    let inputs = createOutputLst nSize [0, 1, 2, 3]
    let outputsXOR = createOutputLst nSize [0, 3, 3, 0]
    let trainSetXOR = zipWith TrainElem inputs outputsXOR
    let updates = updatesWithConditions (length nn) (length trainSetXOR) 0
    --- To be tested
    let nn0 = trainCANNN nn updates trainSetXOR
    let result = map (== 0) $ distanceCANNN nn0 trainSetXOR
    --- Finish testing
    printResult result

trainCANNNTest_Ex2NN :: IO ()
trainCANNNTest_Ex2NN = do
    print "trainCANNNTest_Ex2NN"
    let nSize = 3
    let nn = [createZNeuron nSize nSize, createZNeuron 2 nSize, createZNeuron nSize 2]
    let inputs = createOutputLst nSize [0, 2, 4, 6, 1, 3, 5, 7]
    let outputs = createOutputLst nSize [0, 6, 7, 3, 2, 4, 2, 1]
    let trainSet = zipWith TrainElem inputs outputs
    let updates = updatesWithConditions (length nn) (length trainSet) 0
    --- To be tested
    -- trainUntilLearned nn trainSet 0 6
    nn' <- recursiveNN trainSet updates nn
    let result = map (== 0) $ distanceCANNN nn' trainSet
    --- Finish testing
    printResult result

trainUntilLearnedTest :: IO ()
trainUntilLearnedTest = do
    print "trainUntilLearnedTest"
    let (nSize, testSize, nnSize) = (4, 5, 3)
    let canN0 = createZNeuron nSize nSize
    let inputs = createOutputLst nSize [1 .. testSize]
    let baseOLst = map (sin . fromIntegral) [1 .. testSize]
    let outputs = createOutputLst nSize $ map (round . (*100)) baseOLst
    let trainSet = zipWith TrainElem inputs outputs
    let nn = replicate nnSize canN0
    let updates = take (length trainSet) $ constructUpdate (length nn)
    --- To be tested
    nn' <- trainUntilLearned nn trainSet 0 0
    let result = map (== 0) $ distanceCANNN nn' trainSet
    --- Finish testing

    print result

applyDeltaThresholdTest :: IO ()
applyDeltaThresholdTest = do
    print "applyDeltaThresholdTest"
    let nSize =  4
    let baseT = [1,2,3,4]
    let baseD = [1,0,0,0]
    let baseE = [2,2,3,4]
    let maxVal = 4
    --- To be tested
    let result = [ applyDeltaThresholdOne nSize (-1) baseT baseD 0 baseE maxVal,
                   applyDeltaThresholdOne nSize 0 baseT baseD 0 baseE maxVal,
                   applyDeltaThresholdOne nSize 2 baseT baseD 0 baseE maxVal,
                   applyDeltaThresholdOne nSize 3 baseT baseD 0 baseE maxVal,
                   applyDeltaThresholdOne nSize 4 baseT baseD 0 baseE maxVal,
                   applyDeltaThresholdOne nSize 2 baseT [0,-1,0,0] 1 [1,1,3,4] maxVal,
                   applyDeltaThresholdOne nSize 1 baseT [0,-1,-1,0] 2 [1,2,2,4] maxVal,
                   applyDeltaThresholdOne 3 1 [1,2,3] [1,0,0] 0 [2,2,3] maxVal,
                   applyDeltaThresholdOne 3 1 [1,2,2] [1,0,-1] 2 [1,2,1] maxVal,
                   applyDeltaThresholdOne 3 1 [1,2,3] [1,0,0] 0 [2,2,3] maxVal,
                   applyDeltaThresholdOne 3 0 [1,2,3] [0,0,1] 2 [1,2,3] 3,
                   applyDeltaThresholdOne 3 0 [1,2,0] [0,0,-1] 2 [1,2,0] 3,
                   applyDeltaThresholdOne 3 2 [4,2,0] [1,0,-1] 0 [2,2,0] 2]
    --- Finish testing
    printResult result

applyDeltaWeightTest :: IO ()
applyDeltaWeightTest = do
    print "applyDeltaWeightTest"
    --- To be tested
    result <- mapM (applyDeltaOne False) testExamples2
    --- Finish testing
    printResult result

deltaNextChangeTest :: IO ()
deltaNextChangeTest = do
    print "deltaNextChangeTest"
    --- To be tested
    let (row, col, baseDelta) = (3, 3, [1,0,0,1,0,1,0,0,1])
    let result = [deltaNextChangeOne row col 1 baseDelta 2 [0,0,0,0,0,1,0,0,1],
                  deltaNextChangeOne row col 2 baseDelta 0 [1,0,0,1,0,0,0,0,0],
                  deltaNextChangeOne row col 0 baseDelta 2 [0,0,0,0,0,1,0,0,1],
                  deltaNextChangeOne row col 4 baseDelta 0 [1,0,0,1,0,0,0,0,0],
                  deltaNextChangeOne row col (-1) baseDelta 0 [1,0,0,1,0,0,0,0,0],
                  deltaNextChangeOne row col 2 [0,0,0,0,0,0,0,0,0] (-1) [0,0,0,0,0,0,0,0,0]
                  ]
    --- Finish testing
    printResult result

thresholdIndexChangeTest :: IO ()
thresholdIndexChangeTest = do
    print "thresholdIndexChangeTest"
    --- To be tested
    let (rows, example) = (4, [-1,0,1,-1])
    let result = [thresholdIndexChangeOne 2 rows example (Just 3),
                  thresholdIndexChangeOne 1 rows example (Just 2),
                  thresholdIndexChangeOne 0 rows example (Just 2),
                  thresholdIndexChangeOne 3 rows example (Just 0),
                  thresholdIndexChangeOne 4 rows example (Just 0),
                  thresholdIndexChangeOne 5 rows example (Just 0),
                  thresholdIndexChangeOne (-1) rows example (Just 0),
                  thresholdIndexChangeOne 1 rows [0,0,0,0] Nothing,
                  thresholdIndexChangeOne 6 rows [0,0,-1,0] (Just 2)]
    --- Finish testing
    printResult result

weightIndexChangeTest :: IO ()
weightIndexChangeTest = do
    print "weightIndexChangeTest"
    --- To be tested
    let (rows, example) = (5, [0,1,1,0,0])
    let result = [weightIndexChangeOne 2 rows example (Just 1),
                  weightIndexChangeOne 1 rows example (Just 2),
                  weightIndexChangeOne 0 rows example (Just 1),
                  weightIndexChangeOne 3 rows example (Just 1),
                  weightIndexChangeOne 4 rows example (Just 1),
                  weightIndexChangeOne (-1) rows example (Just 1),
                  weightIndexChangeOne 1 rows [0,0,0,0] Nothing,
                  weightIndexChangeOne (-1) rows [0,0,0,0] Nothing]
    --- Finish testing
    printResult result

splitVecAtTest :: IO ()
splitVecAtTest = do
    print "splitVecAtTest"
    -- To be tested
    let result = [splitVecAtOne (-1) [1,2,3] ([-1,-1,-1], [1,2,3]),
                  splitVecAtOne 0 [1,2,3] ([1], [2,3]),
                  splitVecAtOne 5 [1,2,3] ([1,2,3], [-1,-1,-1]),
                  splitVecAtOne 1 [1,2,3,4,5] ([1,2], [3,4,5])]
    -- Finish testing
    printResult result

trainWithEpochsTest :: IO ()
trainWithEpochsTest = do
    print "trainWithEpochsTest"
    let input = createOutputLst 3 [0, 2, 4, 6, 1, 3, 5, 7]
    let output = createOutputLst 3 [0, 6, 7, 3, 2, 4, 2, 1]
    let trainSet = zipWith TrainElem input output
    let canN0 = createZNeuron 3 3
    let canN1 = createZNeuron 2 3
    let canN2 = createZNeuron 3 2
    let nn = [canN0, canN1, canN2]
    let eN0 = CANWElem 1 $ createWeight 3 3 [1,0,0,0,1,0,1,0,0]
    let eN1 = CANWElem 2 $ createWeight 2 3 [1,0,1,1,1,1]
    let eN2 = CANWElem 0 $ createWeight 3 2 [0,0,1,0,1,1]
    let eT0 = CANTElem 2 $ createThreshold 3 0 [3,2,2]
    let eT1 = CANTElem 1 $ createThreshold 2 0 [1,2]
    let eT2 = CANTElem 2 $ createThreshold 3 0 [2,1,2]
    let expected = [CANNeuron eN0 eT0, CANNeuron eN1 eT1, CANNeuron eN2 eT2]
    let updates = applyNTimes shiftLeft 2 $ constructUpdate 3
    -- To be tested
    nn' <- trainWithEpochs nn trainSet 0 1
    nn'' <- trainWithEpochs nn' trainSet 2 1
    nn''' <- trainWithEpochs nn trainSet 0 2
    let nn0' = trainNeurons (head trainSet) (head updates) nn'
    let nn1' = trainNeurons (trainSet !! 1) (updates !! 1) nn0'
    let nn2' = trainNeurons (trainSet !! 2) (updates !! 2) nn1'
    let nn3' = trainNeurons (trainSet !! 3) (updates !! 3) nn2'
    let nn4' = trainNeurons (trainSet !! 4) (updates !! 4) nn3'
    let nn5' = trainNeurons (trainSet !! 5) (updates !! 5) nn4'
    let nn6' = trainNeurons (trainSet !! 6) (head updates) nn5'
    let nn7' = trainNeurons (trainSet !! 7) (updates !! 1) nn6'
    let result = [nn'' == expected,
                  nn'' == nn7',
                  nn7' == expected,
                  nn''' == expected]
    -- print nn''
    -- print nn'''
    -- Finish testing
    printResult result

trainUntilLearnedTest2 :: IO ()
trainUntilLearnedTest2 = do
    print "trainUntilLearnedTest2"
    let input = createOutputLst 4 [0 .. 15]
    let output = createOutputLst 5 [y * 2 | y <- [0 .. 15]]
    let trainSet = zipWith TrainElem input output
    let canN0 = createZNeuron 3 4
    let canN1 = createZNeuron 2 3
    let canN2 = createZNeuron 5 2
    let nn = [canN0, canN1, canN2]
    let eN0 = CANWElem 2 $ createWeight 3 4 [0,0,0,1,0,0,1,0,1,1,1,0]
    let eN1 = CANWElem 2 $ createWeight 2 3 [0,1,0,0,0,0]
    let eN2 = CANWElem 0 $ createWeight 5 2 [1,1,1,1,0,0,1,0,1,1]
    let eT0 = CANTElem 1 $ createThreshold 3 0 [3,3,2]
    let eT1 = CANTElem 0 $ createThreshold 2 0 [2,1]
    let eT2 = CANTElem 3 $ createThreshold 5 0 [1,1,2,2,2]
    let expected = [CANNeuron eN0 eT0, CANNeuron eN1 eT1, CANNeuron eN2 eT2]
    let expDist = [1,2,0,1,2,1,1,2,2,1,1,2,1,0,2,1]
    let expMatch = 2
    let updates = applyNTimes shiftLeft 2 $ constructUpdate 3
    -- To be tested
    nn' <- trainUntilLearned nn trainSet 0 20
    let dist = distanceCANNN nn' trainSet
    let matches = length . filter (== 0) $ dist
    let result = [nn' == expected,
                  dist == expDist,
                  matches == expMatch]
    -- Finish testing
    -- print nn'
    -- print expected
    -- print dist
    -- print matches
    printResult result

--------------------------------------------------------------------------------
---------- Extra Methods ----------
--------------------------------------------------------------------------------

layerColideOne :: (Int, Int) -> [Int] -> [Int] -> [Int] -> Bool
layerColideOne (row, col) weightLst inputLst expectedLst = result
    where oneCANWElem xs = CANWElem 0 $ createWeight row col xs
          weight = oneCANWElem weightLst
          input = createOutput col inputLst
          expected = createWeight row col expectedLst
          --- To be tested
          result = expected == R.computeS (layerColide weight input xor)
          --- Finish testing

applyDeltaOne :: Bool -> ((Int, Int),(Int, [Int]),[Int], (Int, [Int])) -> IO Bool
applyDeltaOne display ((row, col), (indexW, weightLst), deltaLst, (indexE, expectedLst)) = do
    let weights = CANWElem indexW $ createWeight row col weightLst
    let delta = createWeight row col deltaLst
    let expected = CANWElem indexE $ createWeight row col expectedLst
    let result = applyDeltaWeight weights (R.delay delta)
    when display $ do
        print weights
        print (CANWElem 0 delta)
        print expected
        print result
        print "*********"
    return $ result == expected

applyDeltaThresholdOne :: Int -> Int -> [Int] -> [Int] -> Int -> [Int] -> Int -> Bool
applyDeltaThresholdOne tSize index thresholdLst deltaLst expIndex expectedLst maxValue = result
    where threshold = CANTElem index $ createThreshold tSize 0 thresholdLst
          delta = createThreshold tSize 0 deltaLst
          expected = CANTElem expIndex $ createThreshold tSize 0 expectedLst
          --- To be tested
          result = expected == applyDeltaThreshold threshold (R.delay delta) maxValue

deltaNextChangeOne :: Int -> Int -> Int -> [Int] -> Int -> [Int] -> Bool
deltaNextChangeOne row col indexW weightLst indexE expectedLst = result
    where delta = createWeight row col weightLst
          expected = createWeight row col expectedLst
          --- To be tested
          result = (indexE, R.delay expected) == deltaNextChange (R.delay delta) indexW
          --- Finish testing

thresholdIndexChangeOne :: Int -> Int -> [Int] -> Maybe Int -> Bool
thresholdIndexChangeOne index row threshold expected = result == expected
    where nnt = createThreshold row 0 threshold
          --- To be tested
          result = thresholdIndexChange index (R.delay nnt)
          --- Finish testing

weightIndexChangeOne :: Int -> Int -> [Int] -> Maybe Int -> Bool
weightIndexChangeOne index row weights expected = result == expected
    where nnt = createOutput row weights
          --- To be tested
          result = weightIndexChange index (R.delay nnt)
          --- Finish testing

splitVecAtOne :: Int -> [Int] -> ([Int], [Int]) -> Bool
splitVecAtOne location vecLst (expectedFLst, expectedSLst) = result == expected
    where vec = R.delay $ createThreshold (length vecLst) 0 vecLst
          expected = (R.delay $ createThreshold (length expectedFLst) 0 expectedFLst,
                      R.delay $ createThreshold (length expectedSLst) 0 expectedSLst)
          --- To be tested
          result = splitVecAt location vec
          --- Finish testing


printPartialNN :: TrainElem -> CANUpdate -> [CANNeuron] -> IO [CANNeuron]
printPartialNN train update nn = do
    let nn' = trainNeurons train update nn
    print nn'
    print "------------"
    return nn'

recursiveNN :: [TrainElem] -> [CANUpdate] -> [CANNeuron] -> IO [CANNeuron]
recursiveNN [] _ nn = return nn
recursiveNN (x : xs) (y : ys) nn = do
    nn' <- printPartialNN x y nn
    recursiveNN xs ys nn'

checkFails :: [Bool] -> String
checkFails xs = show count ++ " / " ++ show (length xs)
    where count = foldl (\x y -> bool x (x + 1) y) 0 xs

printResult :: [Bool] -> IO ()
printResult xs = do
    print xs
    print $ checkFails xs
    print "------------"

--------------------------------------------------------------------------------
---------- Extra Methods ----------
--------------------------------------------------------------------------------

testExamples :: [(Int, Int, [Int], [Int], [Int])]
testExamples = zip5 row col weights input wXORi
    where row = [2, 3, 3, 2, 2, 2]
          col = [3, 2, 3, 3, 2, 2]
          weights = [[1,0,0,1,1,0], [1,0,1,0,0,1], [1,0,1,0,0,0,1,0,1], [0,0,0,1,1,0],
                    [1,0,1,0], [0,1,0,1]]
          input = [[0, 0, 0], [1,0], [1,0,1], [0,0,1],
                  [0,0], [0,1]]
          wXORi = [[1, 0, 0, 1, 1, 0], [0,0,0,0,1,1], [0,0,0,1,0,1,0,0,0], [0,0,1,1,1,1],
                  [1,0,1,0], [0,0,0,0]]

testExamples2 :: [((Int, Int), (Int, [Int]), [Int], (Int, [Int]))]
testExamples2 = [ex1, ex2, ex3, ex4, ex5, ex6]
    where ex1 = ((2, 3), (0, []), [1,1,1,1,1,1], (1, [0,1,0,0,1,0]))
          ex2 = ((3, 2), (1, []), [0,0,0,1,0,1], (1, [0,0,0,1,0,1]))
          ex3 = ((3, 3), (1, []), [0,0,0,0,0,0,1,0,0], (0, [0,0,0,0,0,0,1,0,0]))
          ex4 = ((3, 2), (1, []), [0,0,1,0,0,1], (0,[0,0,1,0,0,0]))
          ex5 = ((3, 3), (1, []), [0,0,0,0,0,0,1,0,1], (2, [0,0,0,0,0,0,0,0,1]))
          ex6 = ((3, 3), (1, []), [0,1,0,0,0,0,1,0,1], (2, [0,0,0,0,0,0,0,0,1]))

