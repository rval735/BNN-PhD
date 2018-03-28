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
type NNT = Double
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
data NeuralNetworkR = NeuralNetworkR {
    lRateR :: LearningRate,
    wihR   :: NNLayerU,
    whoR   :: NNLayerU
} deriving (Show)

-- | Take a NNBase, then create a NeuralNetwork from its parameters
--   Impure function becase it uses random numbers
createNNR :: NNBase -> IO NeuralNetworkR
createNNR (NNBase x y z lr) = do
    randomSeed <- randomIO :: IO Int
    let pRN = randomNormalR randomSeed
    return $ NeuralNetworkR lr (pRN y x) (pRN z y)

-- | Impure function that generates a normalized random matrix of doubles
--   considering input - hidden layers
randomNormalR :: Int ->  InputNodes -> OutputNodes -> NNLayerU
randomNormalR seed x y = randomishDoubleArray shape (-stdDev) stdDev seed
    where shape = Z :. x :. y
          stdDev = 2 * fromIntegral x ** (-0.5)

-- | Vector application of the "Logistic" activation function
activationFuncR :: (Shape sh, Monad m) => Array U sh NNT -> m (Array U sh NNT)
activationFuncR v = computeP $ map logisticFunc v

-- | Logistic function formula, taking R and returning R
logisticFunc :: NNT -> NNT
logisticFunc x = 1 / (1 + exp (-x))

-- | Match training steps from the Python example
trainR :: NLayerU -> NLayerU -> NeuralNetworkR -> IO NeuralNetworkR
trainR inputs training (NeuralNetworkR lRateNN wihNN whoNN) =
    do
    -- Multiply training inputs against input weights
        hiddenInputs <- matVecDense wihNN inputs
    -- Run the activation function from the result
        hiddenOutputs <- activationFuncR hiddenInputs
    -- Multiply activated training inputs against output weights
        finalInputs <- matVecDense whoNN hiddenOutputs
    -- Run the activation function from the output weights result
        finalOutputs <- activationFuncR finalInputs
    -- Match the NN prediction agains the expected training value
        outputErrors <- computeP $ training -^ finalOutputs -- :: IO NLayerU
    -- Transpose the output to match the hidden inputs
        whoNNT <- computeP $ transpose whoNN -- :: IO NNLayerU
    -- Multiply the difference error with the NN output weights
        hiddenErrors <- matVecDense whoNNT outputErrors
    -- Calculate a "gradient" with the expected training value, the
    -- NN calculated output, which is multiplied by the hidden NN outputs
        preWHO <- kerMap outputErrors finalOutputs hiddenOutputs
    -- Apply the learning rate to the newly calculated output weights
        whoDelta <- computeP . transpose $ map (lRateNN *) preWHO :: IO NNLayerU
    -- Make the "gradient" but in this case for the input weights
        preWIH <- kerMap hiddenErrors hiddenOutputs inputs
    -- Apply the learning rate to the newly calculated input weights
        wihDelta <- computeP . transpose $ map (lRateNN *) preWIH :: IO NNLayerU
    -- Update inner layers
        wihFin <- computeP $ wihNN +^ wihDelta
        whoFin <- computeP $ whoNN +^ whoDelta
    -- Create a new NN with updated input/output weights
        return $ NeuralNetworkR lRateNN wihFin whoFin

-- | Match training steps from the Python example
trainR' :: (Monad m) => NLayerU -> NLayerU -> NeuralNetworkR -> m NeuralNetworkR
trainR' inputs training (NeuralNetworkR lRateNN wihNN whoNN) =
    do
    -- Multiply training inputs against input weights
        let hiddenInputs = matVecDense'' wihNN inputs
    -- Run the activation function from the result
        let hiddenOutputs = activationFuncR' hiddenInputs
    -- Multiply activated training inputs against output weights
        let finalInputs = matVecDense'' whoNN hiddenOutputs
    -- Run the activation function from the output weights result
        let finalOutputs = activationFuncR' finalInputs
    -- Match the NN prediction agains the expected training value
        let outputErrors = training -^ finalOutputs -- :: IO NLayerU
    -- Transpose the output to match the hidden inputs
        let whoNNT = transpose whoNN -- :: IO NNLayerU
    -- Multiply the difference error with the NN output weights
        let hiddenErrors = matVecDense'' whoNNT outputErrors
    -- Calculate a "gradient" with the expected training value, the
    -- NN calculated output, which is multiplied by the hidden NN outputs
        let preWHO = kerMap'' outputErrors finalOutputs hiddenOutputs
    -- Apply the learning rate to the newly calculated output weights
        let whoDelta  = transpose $ map (lRateNN *) preWHO
    -- Make the "gradient" but in this case for the input weights
        let preWIH = kerMap'' hiddenErrors hiddenOutputs inputs
    -- Apply the learning rate to the newly calculated input weights
        let wihDelta = transpose $ map (lRateNN *) preWIH
    -- Update inner layers
        wihFin <- computeP $ wihNN +^ wihDelta
        whoFin <- computeP $ whoNN +^ whoDelta
    -- Create a new NN with updated input/output weights
        return $ NeuralNetworkR lRateNN wihFin whoFin

-- | Match query steps from the Python example
queryR :: NeuralNetworkR -> NLayerU -> IO NLayerU
queryR (NeuralNetworkR lRateNN wihNN whoNN) inputs = do
    -- Multiply training inputs against input weights
        hiddenInputs <-  matVecDense wihNN inputs
    -- Run the activation function from the result
        hiddenOutputs <- activationFuncR hiddenInputs
    -- Multiply activated training inputs against output weights
        finalInputs <- matVecDense whoNN hiddenOutputs
    -- Execute the activation functor to final inputs
        activationFuncR finalInputs

--------------------------------------
--------- Helper Functions -----------
--------------------------------------

matVecDense ::  NNLayerU -> NLayerU -> IO NLayerU
matVecDense x y = do
    let (Z :. s1 :. s2) = extent x
    extended <- computeP $ extend (Any :. s1 :. All) y :: IO NNLayerU
    pre <- computeP $ x *^ extended :: IO NNLayerU
    sumP pre

-- matVecDense' :: ((Shape sh, Source r a, Monad m) =>  NNLayerU -> NLayerU -> m NLayerU
-- matVecDense' :: (Shape sh, Source r a, Monad m) => NLayerN r -> NLayerN r -> m NLayerU
 -- matVecDense' :: (Shape sh1, Source r1 a, Shape sh2, Source r2 a, Num a, Monad m) => Array r1 (sh1 :. Int :. Int) a -> Array r2 (sh2 :. Int) a -> Array D sh2 a
matVecDense' :: (Monad m) =>  NNLayerU -> NLayerU -> m NLayerU
matVecDense' x y = sumP pre
    where (Z :. s1 :. s2) = extent x
          extended = extend (Any :. s1 :. All) y
          pre = x *^ extended

-- matVecDense'' :: (Shape sh1, Source r1 a, Shape sh2, Source r2 a, Num a, Unbox a, Monad m) => Array r1 (sh1 :. Int :. Int) a -> Array r2 (sh2 :. Int) a -> m (Array U sh2 a)
{-# INLINE matVecDense'' #-}
matVecDense'' :: (Source r1 NNT, Source r2 NNT) => NNLayerF r1 -> NLayerF r2 -> NLayerU
matVecDense'' x y = do
    let applySHY f h sh@(Z :. x :. y) = f sh * h (Z :. y)
    let pre = traverse2 x y const applySHY
    sumS pre

matSumVecDense ::  NNLayerU -> NLayerU -> IO NNLayerU
matSumVecDense x y = do
    let (Z :. sy) = extent y
    extended <- computeP $ extend (Any :. sy :. All) y :: IO NNLayerU
    computeP $ x +^ extended

matSumVecDense' :: NNLayerU -> NLayerU -> NNLayerD
matSumVecDense' x y = do
    let (Z :. sy) = extent y
    let extended = extend (Any :. sy :. All) y
    x +^ extended

outerProd :: NLayerU -> NLayerU -> IO NNLayerU
outerProd x y = do
    let (Z :. sx) = extent x
    let (Z :. sy) = extent y
    extendX <- computeP $ extend (Any :. sy :. All) x :: IO NNLayerU
    extendY <- computeP . transpose $ extend (Any :. sx :. All) y :: IO NNLayerU
    computeP $ extendX *^ extendY -- :: IO NNLayerU

outerProd' :: NLayerD -> NLayerU -> NNLayerD
outerProd' x y = do
    let (Z :. sx) = extent x
    let (Z :. sy) = extent y
    let extendX = extend (Any :. sy :. All) x
    let extendY = transpose $ extend (Any :. sx :. All) y
    extendX *^ extendY -- :: IO NNLayerU

minMap :: NNT -> NLayerU -> IO NLayerU
minMap val layer = computeP $ map (val -) layer

{-# INLINE minMap' #-}
minMap' :: NNT -> NLayerU -> NLayerD
minMap' val = map (val -)

kerMap :: NLayerU -> NLayerU -> NLayerU -> IO NNLayerU
kerMap outErr outs hidden = do
    minusOutputs <- minMap 1 outs
    preKer <- computeP $ outErr *^ outs  *^ minusOutputs
    outerProd preKer hidden

kerMap' :: NLayerU -> NLayerU -> NLayerU -> NNLayerD
kerMap' outErr outs hidden = do
    let minusOutputs = minMap' 1 outs
    let preKer = outErr *^ outs  *^ minusOutputs
    outerProd' preKer hidden

{-# INLINE kerMap'' #-}
kerMap'' :: (Source r1 NNT, Source r2 NNT, Source r3 NNT) => NLayerF r1 -> NLayerF r2 -> NLayerF r3 -> NNLayerD
kerMap'' outErr outs hidden = do
    let minusOutputs = minMap'' 1 outs
    let preKer = outErr *^ outs  *^ minusOutputs
    outerProd'' preKer hidden

{-# INLINE minMap'' #-}
minMap'' :: (Source r NNT) => NNT -> NLayerF r -> NLayerD
minMap'' val = map (val -)

{-# INLINE outerProd'' #-}
outerProd'' :: (Source r1 NNT, Source r2 NNT) => NLayerF r1 -> NLayerF r2 -> NNLayerD
outerProd'' x y = do
    let (Z :. sx) = extent x
    let (Z :. sy) = extent y
    let extendX = extend (Any :. sy :. All) x
    let extendY = transpose $ extend (Any :. sx :. All) y
    extendX *^ extendY -- :: IO NNLayerU

{-# INLINE activationFuncR' #-}
-- | Vector application of the "Logistic" activation function
activationFuncR' :: (Source r NNT, Shape sh) => Array r sh NNT -> Array D sh NNT
activationFuncR' = map logisticFunc
