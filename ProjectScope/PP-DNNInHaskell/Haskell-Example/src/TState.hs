----
---- CNN-PhD version 0.1, Copyright (C) 5/Jun/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----


module TState
-- (
-- )
where

import           System.Random

data TState = Mn | Zn | Pn
    deriving (Eq, Ord, Show)


plusTState :: TState -> TState
plusTState Mn = Zn
plusTState _  = Pn

minusTState :: TState -> TState
minusTState Pn = Zn
minusTState _  = Mn


-----------------------------------------
----------- Random Instance -------------
-----------------------------------------
instance Random TState where
    randomR (l, h) g
        | l >= h = (Zn, snd $ next g)
        | otherwise = until (\(x, gX) -> testRange l h x) (\(x, gX) -> randomTState gX) (randomTState g)
    random = randomTState

testRange :: (Ord a) => a -> a -> a -> Bool
testRange x y z = (z >= x) && (z <= y)

randomTState :: RandomGen g => g -> (TState, g)
randomTState g = (quantizationFunction quarter (mid - val), g')
    where (minX, maxX) = genRange g
          quarter = floor $ fromIntegral (maxX - minX) / 4
          mid = floor $ fromIntegral (maxX - minX) / 2
          (val, g') = next g

-----------------------------------------
-----------------------------------------

quantizationFunction :: Int -> Int -> TState
quantizationFunction threshold value
    | value > threshold = Pn
    | value < (-threshold) = Mn
    | otherwise = Zn

quantizationDerivate :: Int -> Int -> Int -> Float
quantizationDerivate value threshold steep = res
    where absVal = abs value
          tms = threshold - steep
          tps = threshold + steep
          withinRange = absVal >= tms && absVal <= tps
          res | withinRange = 1.0 / (2.0 * fromIntegral steep)
              | otherwise = 0.0
