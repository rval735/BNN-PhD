----
---- CNN-PhD version 0.1, Copyright (C) 3/Mar/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Simple Neural Network class that has one hidden layer
module NNClass where

import           Data.Array.Repa
import           Data.Array.Repa.Algorithms.Randomish (randomishDoubleArray)
import           Prelude                              hiding (map)
import           System.Random                        (randomIO)

-- | Code synonyms to ease relationship between function
--   parameters and their application
type NNT = Bool
type InputNodes = Int
type OutputNodes = Int
type HiddenNodes = Int
type Target = Int
type LearningRate = NNT
type Epochs = Int

-- | Synonyms for dimensions
type NShape = DIM1 -- Z :. InputNodes
type NNShape = DIM2 -- Z :. InputNodes :. OutputNodes

-- | Synonyms for Array types used in NNClass
type RLayerFlex r sh = Array r sh NNT
type RLayerU sh = RLayerFlex U sh
type RLayerD sh = RLayerFlex D sh
type NLayerD = RLayerD NShape
type NNLayerD = RLayerD NNShape
type NLayerU = RLayerU NShape
type NNLayerU = RLayerU NNShape
type NLayerF r = RLayerFlex r NShape
type NNLayerF r = RLayerFlex r NNShape

-- | Data definition that serves as a base line for NN creation
data NNBase = NNBase {
    inodes    :: InputNodes,
    hnodes    :: HiddenNodes,
    onodes    :: OutputNodes,
    baseLRate :: LearningRate
} deriving (Show)

-- | Kind of "instanced" NNBase where input/output weights have
--   been assigned along with a learning rate
data NeuralNetwork = NeuralNetwork {
    lRateR :: LearningRate,
    wihR   :: NNLayerU,
    whoR   :: NNLayerU
} deriving (Show)

-- | Take a NNBase, then create a NeuralNetwork from its parameters
--   Impure function becase it uses random numbers
createNNR :: NNBase -> IO NeuralNetwork
createNNR (NNBase x y z lr) = do
    randomSeed <- randomIO :: IO Int
    let pRN = randomNormalR randomSeed
    return $ NeuralNetwork lr (pRN y x) (pRN z y)

-- | Impure function that generates a normalized random matrix of doubles
--   considering input - hidden layers
randomNormalR :: Int ->  InputNodes -> OutputNodes -> NNLayerU
randomNormalR seed x y = computeUnboxedS $ map (> 0) doubleArr
    where shape = Z :. x :. y
          stdDev = 2 * fromIntegral x ** (-0.5)
          doubleArr = randomishDoubleArray shape (-stdDev) stdDev seed

{-# INLINE activationFuncR #-}
-- | Vector application of the "Logistic" activation function
activationFuncR :: (Source r NNT, Shape sh) => Array r sh NNT -> Array D sh NNT
activationFuncR ys = traverse2 ys ys const (\f g sh -> xnor (f sh) (g sh))
    where  -- | Logistic function formula, taking R and returning R
          xnor :: Bool -> Bool -> Bool
          xnor x y = x == y

-- | Match training steps from the Python example
train :: (Monad m) => NLayerU -> NLayerU -> NeuralNetwork -> m NeuralNetwork
train inputs training (NeuralNetwork lRateNN wihNN whoNN) = do
-- Multiply training inputs against input weights
    let hiddenInputs = matVecDense wihNN inputs
-- Run the activation function from the result
    let hiddenOutputs = activationFuncR hiddenInputs
-- Multiply activated training inputs against output weights
    let finalInputs = matVecDense whoNN hiddenOutputs
-- Run the activation function from the output weights result
    let finalOutputs = activationFuncR finalInputs
-- Match the NN prediction agains the expected training value
    let outputErrors = training -^ finalOutputs -- :: IO NLayerU
    -- let outputErrors = minusBinOpr training finalOutputs -- :: IO NLayerU
-- Transpose the output to match the hidden inputs
    let whoNNT = transpose whoNN -- :: IO NNLayerU
-- Multiply the difference error with the NN output weights
    let hiddenErrors = matVecDense whoNNT outputErrors
-- Calculate a "gradient" with the expected training value, the
-- NN calculated output, which is multiplied by the hidden NN outputs
    let preWHO = kerMap outputErrors finalOutputs hiddenOutputs
-- Apply the learning rate to the newly calculated output weights
    let whoDelta  = transpose $ map (lRateNN *) preWHO
-- Make the "gradient" but in this case for the input weights
    let preWIH = kerMap hiddenErrors hiddenOutputs inputs
-- Apply the learning rate to the newly calculated input weights
    let wihDelta = transpose $ map (lRateNN *) preWIH
-- Update inner layers
    wihFin <- computeP $ wihNN +^ wihDelta
    whoFin <- computeP $ whoNN +^ whoDelta
        -- wihFin <- plusBinOpr wihNN wihDelta
        -- whoFin <- plusBinOpr whoNN whoDelta
-- Create a new NN with updated input/output weights
    return $ NeuralNetwork lRateNN wihFin whoFin

-- | Match query steps from the Python example
query :: (Monad m) => NeuralNetwork -> NLayerU -> m NLayerU
query (NeuralNetwork lRateNN wihNN whoNN) inputs = do
-- Multiply training inputs against input weights
    let hiddenInputs = matVecDense wihNN inputs
-- Run the activation function from the result
    let hiddenOutputs = activationFuncR hiddenInputs
-- Multiply activated training inputs against output weights
    let finalInputs = matVecDense whoNN hiddenOutputs
-- Run the activation function from the output weights result
    computeP $ activationFuncR finalInputs

-------------------------------------------
--------- Repa Matrix Functions -----------
-------------------------------------------

-- | Make a Matrix - Vector multiplication that will obtain a Vector
--   Ex: | 0 1 2 |    | 1 |       | 8  |
--       | 3 4 5 |    | 2 |   =   | 26 |
--       | 6 7 8 |    | 3 |       | 44 |
{-# INLINE matVecDense #-}
matVecDense :: (Source r1 NNT, Source r2 NNT) => NNLayerF r1 -> NLayerF r2 -> NLayerU
matVecDense x y = foldS (+) False $ traverse2 x y const applySHY
    where applySHY f h sh@(Z :. x :. y) = f sh * h (Z :. y)

-- | Transforms two Vector objects into a Matrix
--   with columns of V2 and rows V1
{-# INLINE outerProd #-}
outerProd :: (Source r1 NNT, Source r2 NNT) => NLayerF r1 -> NLayerF r2 -> NNLayerD
outerProd x y = do
    let (Z :. sx) = extent x
    let (Z :. sy) = extent y
    let extendX = extend (Any :. sy :. All) x
    let extendY = transpose $ extend (Any :. sx :. All) y
    extendX *^ extendY

-- | Constant minus a Vector
{-# INLINE minMap #-}
minMap :: (Source r NNT) => NNT -> NLayerF r -> NLayerD
minMap val = map (val -)

-- | Helper that performs multiple functions to three vectors: multiply the first
--  to the second at the same time that "1 complement" of the second. With that
--  result it applies the outer product to the third element.
{-# INLINE kerMap #-}
kerMap :: (Source r1 NNT, Source r2 NNT, Source r3 NNT) => NLayerF r1 -> NLayerF r2 -> NLayerF r3 -> NNLayerD
kerMap outErr outs hidden = do
    let minusOutputs = minMap True outs
    let preKer = outErr *^ outs  *^ minusOutputs
    outerProd preKer hidden

instance Num Bool where
    (+) = (||)
    negate = not
    (*) = (&&)
    abs _ = True
    signum x = x
    fromInteger x = 1
