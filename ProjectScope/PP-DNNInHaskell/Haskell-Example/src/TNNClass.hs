----
---- CNN-PhD version 0.1, Copyright (C) 5/Jun/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- | Tri-State Neural Network class
module TNNClass
-- (
-- )
where

import           Data.Bool        (bool)
import           Data.List.Unique (countElem)
import           TState

txnor :: TState -> TState -> TState -> TState
txnor Zn _ _ = Zn
txnor _ inX inW
    | inX == inW = Pn
    | otherwise = Mn

gxnor :: TState -> TState -> TState
gxnor inX inW = txnor (nonZero inX inW) inX inW

sxnor :: TState -> TState -> TState
sxnor x y
    | x == y = Pn
    | y > x = Mn
    | otherwise = Zn

nonZero :: TState -> TState -> TState
nonZero Zn _ = Zn
nonZero _ Zn = Zn
nonZero _ _  = Pn

signT :: TState -> TState
signT Mn = Mn
signT _  = Pn

shiftTP :: TState -> TState
shiftTP Pn = Mn
shiftTP Zn = Pn
shiftTP Mn = Zn

shiftTN :: TState -> TState
shiftTN Pn = Zn
shiftTN Zn = Mn
shiftTN Mn = Pn

shiftTBy :: TState -> TState -> TState
shiftTBy x Zn = x
shiftTBy x Pn = shiftTP x
shiftTBy x Mn = shiftTN x

tStateTransition :: TState -> TState -> TState
-- tStateTransition Pn Mn = Zn
-- tStateTransition Pn Pn = Pn
-- -- tStateTransition Pn Zn = Mn
-- tStateTransition Mn Pn = Zn
-- tStateTransition Mn Mn = Mn
-- -- tStateTransition Mn Zn = Pn
-- -- tStateTransition Zn Pn = Mn
-- -- tStateTransition Zn Mn = Pn
-- -- tStateTransition Zn Zn = Zn
tStateTransition Zn x  = x
tStateTransition x Zn  = x
tStateTransition x y
    | x == y = x
    | otherwise = Zn

tStateTransit :: TState -> Double -> Double -> TState
tStateTransit x deltaW tau
    | deltaW >= 0 = bool x (plusTState x) (tau > 0.5)
    | deltaW < 0 = bool x (minusTState x) (tau > 0.5)

tStateDiff :: TState -> TState -> TState
tStateDiff x y
    | x == y = Zn
    | x >= y = Pn
    | otherwise = Mn

-- tCount :: [TState] -> TState
-- tCount xs = fst maxVal
--     where counted = count xs
--           filterF m@(_,x) n@(_,y) = if x > y then m else n
--           maxVal = foldr filterF (Zn, 0) counted
tCount :: [TState] -> TState
tCount xs = bool maxVal Zn (countMn == countPn)
    where countMn = countElem Mn xs
          countPn = countElem Pn xs
          maxVal = bool Pn Mn (countMn > countPn)

costFunction :: [TState] -> [TState] -> [TState]
costFunction = zipWith tStateDiff

--------------------------------------------------------------------------------

transitionProb :: Float -> Float -> Float -> Float
transitionProb m v z = tanh (m * (abs v / z))

--------------------------------------------------------------------------------

tStateToInt :: TState -> Int
tStateToInt Mn = -1
tStateToInt Zn = 0
tStateToInt Pn = 1

intToTState :: Int -> TState
intToTState x
    | x <= -1 = Mn
    | x >= 1 = Pn
    | otherwise = Zn

--------------------------------------------------------------------------------

applyFunction :: (t -> a -> b) -> [a] -> [t] -> [[b]]
applyFunction f x y = map (\(x', y') -> map (f y') x') $ matchIndex x y

matchIndex :: a -> [b] -> [(a, b)]
matchIndex x y = zip (replicate (length y) x) y

--------------------------------------------------------------------------------

loopValues :: Int -> [TState] -> [TState] -> [[TState]] -> [[TState]]
loopValues 0 _ _ nn = nn
loopValues epochs expected y nn = do
    let query = queryTNN y nn
    let cF = costFunction query expected
    let updatedNN = updateTNN cF nn []
    loopValues (epochs - 1) expected y updatedNN

evaluateActivation :: [TState] -> [TState] -> [TState]
evaluateActivation weigths layer = map tCount $ applyFunction gxnor layer weigths

evaluateTransition :: [TState] -> [TState] -> [TState]
evaluateTransition weigths layer = map tCount $ applyFunction tStateTransition layer weigths

queryTNN :: [TState] -> [[TState]] -> [TState]
queryTNN = foldl (flip evaluateActivation)

updateTNN :: [TState] -> [[TState]] -> [[TState]] -> [[TState]]
updateTNN _ [] ys = ys
updateTNN [] _ ys = ys
updateTNN update (x:xs) ys = updateTNN updated xs (ys ++ [updated])
    where updated = evaluateTransition x update


