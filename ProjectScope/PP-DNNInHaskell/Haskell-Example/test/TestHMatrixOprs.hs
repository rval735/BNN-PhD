----
---- CNN-PhD version 0.1, Copyright (C) 18/Mar/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE BangPatterns #-}

module TestHMatrixOprs
-- (
--     matrixMultiplication
-- )
where

import qualified Data.ByteString.Lazy.Char8    as C8L
import           Data.Time.Clock               (diffUTCTime, getCurrentTime)
import           NNClass
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Devel
import           Numeric.LinearAlgebra.HMatrix

matrixMultiplication :: IO ()
matrixMultiplication = do
    putStrLn "Test matrixMultiplication"
    elems1 <- randomNormal 10000 10000
    let elemsV0 = tr $ elems1 ? [0]
    let elemsV1 = flatten elemsV0
    let elemsM0 = toRows elems1

    startTime <- getCurrentTime
    let !el1 = elems1 <> elemsV0
    -- print $ size el1
    -- print el1
    midTime <- getCurrentTime
    let !el2 = elems1 #> elemsV1
    -- print $ size el2
    -- print el2
    midTime2 <- getCurrentTime
    let !el3 = map (dot elemsV1) elemsM0
    -- print $ length el3
    -- print el3
    endTime <- getCurrentTime

    let diff1 = diffUTCTime midTime startTime
    let diff2 = diffUTCTime midTime2 midTime
    let diff3 = diffUTCTime endTime midTime2

    print $ size el1
    print $ size el2
    print $ length el3

    -- print el1
    -- print el2
    -- print el3

    print diff1
    print diff2
    print diff3

matrixFunctionApplication :: IO ()
matrixFunctionApplication = do
    putStrLn "Test matrixFunctionApplication"
    elemsM <- randomNormal 10000 10000
    let elemsV = flatten elemsM
    let elemsVV = toRows elemsM

    let mapLogisticM = mapMatrixWithIndex (\_ v -> logisticFunc v)
    let mapLogisticV = mapVectorWithIndex (\_ v -> logisticFunc v)
    let mapLogisticVV = map mapLogisticV

    startTime <- getCurrentTime
    let !elM = mapLogisticM elemsM
    midTime <- getCurrentTime
    let !elV =  mapLogisticV elemsV
    midTime2 <- getCurrentTime
    let !elVV =  mapLogisticVV elemsVV
    endTime <- getCurrentTime


    let diff1 = diffUTCTime midTime startTime
    let diff2 = diffUTCTime midTime2 midTime
    let diff3 = diffUTCTime endTime midTime2

    print $ size elM
    print $ size elV
    print $ length elVV

    -- print el1
    -- print el2
    -- print el3

    print diff1
    print diff2
    print diff3
