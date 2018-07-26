----
---- CNN-PhD version 0.1, Copyright (C) 24/Jul/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Helper file to add random creation of CAM types
module ListExtras
-- (
-- )
where

import           Data.Bits
import           Data.Bool (bool)

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

shiftLeft :: [a] -> [a]
shiftLeft []  = []
shiftLeft [x] = [x]
shiftLeft x   = tail x ++ [head x]

shiftRight :: [a] -> [a]
shiftRight []  = []
shiftRight [x] = [x]
shiftRight x   = last x : init x

applyNTimes :: [a] -> ([a] -> [a]) -> Int -> [a]
applyNTimes x _ 0 = x
applyNTimes x f n = let x' = f x in bool (applyNTimes x' f (n - 1)) x' (n <= 0)

sumTrues :: Num a => Bool -> a -> a
sumTrues cond num = bool num (num + 1) cond

sumBits :: (Bits a, Num a) => [Bool] -> a
sumBits = foldr sumTrues 0

xorPCThreshold :: Bits a => a -> Int -> a -> Bool
xorPCThreshold x y = (<= y) . popCount . xor x

flipElems :: Bits a => a -> a -> a
flipElems = (.|.)

num2Bin :: Int -> String
num2Bin n
    | n >= 0     =  concatMap show . reverse . n2b $ n
    | otherwise  =  concatMap show . reverse . n2b . abs $ n

n2b :: Int -> [Int]
n2b 0 =  []
n2b n =  n `mod` 2 : n2b (n `div` 2)

num2Bin' :: Int -> Int -> [Bool]
num2Bin' _ 0 = []
num2Bin' size num = map (\x -> bool False True (check x == 0)) $ lst size
    where lst x = take x $ iterate (* 2) 1
          check x = mod (div (num - x) x) 2

roundExp :: Int -> Int -> Int
roundExp x y = mod opr1 opr2
    where opr1 = round (fromIntegral x / 10 ** fromIntegral y)
          opr2 = round $ 2 ** fromIntegral y

str2Bin :: String -> [Bool]
str2Bin = map (\x -> bool False True ('1' == x))

fixedSize :: Int -> [Bool]-> [Bool]
fixedSize _ [] = []
fixedSize y xs
    | y <= 0 || lstLength == y = xs
    | lstLength > y = reverse . take y . reverse $ xs
    | otherwise = missingPart ++ xs
    where missingPart = replicate (y - lstLength) False
          lstLength = length xs

-- variant of map that passes each element's index as a second argument to f
indexedMap :: (a -> Int -> b) -> [a] -> [b]
indexedMap f l = zipWith f l [0..]

replaceElem :: [a] -> Int -> a -> [a]
replaceElem [] _ _ = []
replaceElem x 0 y = y : tail x
replaceElem x n val
    | n >= length x = x
    | otherwise = xs ++ val : ys
        where (xs, _:ys) = splitAt n x

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
    where (as,bs) = splitAt n xs
