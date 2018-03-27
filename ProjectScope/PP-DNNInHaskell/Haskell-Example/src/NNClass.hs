----
---- CNN-PhD version 0.1, Copyright (C) 3/Mar/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

-- | Simple Neural Network class that has one hidden layer
module NNClass where

import qualified Data.Array.Repa                      as RP
import qualified Data.Array.Repa.Algorithms.Matrix    as RP
import qualified Data.Array.Repa.Algorithms.Randomish as RP
import           System.Random

-- | Code synonyms to ease relationship between function
--   parameters and their application
type NNT = Double
type InputNodes = Int
type OutputNodes = Int
type HiddenNodes = Int
type Target = Int
type LearningRate = NNT
type Epochs = Int

type RGenLayer sh = RP.Array RP.U sh NNT
type RGenLayerD sh = RP.Array RP.D sh NNT
type NShape = RP.DIM1 -- RP.Z RP.:. InputNodes
type NNShape = RP.DIM2 -- RP.Z RP.:. InputNodes RP.:. OutputNodes
type NLayerR = RGenLayer NShape
type NNLayerR = RGenLayer NNShape
type NLayerDR = RGenLayerD NShape
type NNLayerDR = RGenLayerD NNShape


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
    wihR   :: NNLayerR,
    whoR   :: NNLayerR
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
randomNormalR :: Int ->  InputNodes -> OutputNodes -> NNLayerR
randomNormalR seed x y = RP.randomishDoubleArray shape (-stdDev) stdDev seed
    where shape = RP.Z RP.:. x RP.:. y
          stdDev = 2 * fromIntegral x ** (-0.5)

-- | Vector application of the "Logistic" activation function
activationFuncR :: (RP.Shape sh, Monad m) => RP.Array RP.U sh NNT -> m (RP.Array RP.U sh NNT)
activationFuncR v = RP.computeP $ RP.map logisticFunc v

-- | Logistic function formula, taking R and returning R
logisticFunc :: NNT -> NNT
logisticFunc x = 1 / (1 + exp (-x))

-- | Match training steps from the Python example
trainR :: NLayerR -> NLayerR -> NeuralNetworkR -> IO NeuralNetworkR
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
        outputErrors <- RP.computeP $ training RP.-^ finalOutputs -- :: IO NLayerR
    -- Transpose the output to match the hidden inputs
        whoNNT <- RP.computeP $ RP.transpose whoNN -- :: IO NNLayerR
    -- Multiply the difference error with the NN output weights
        hiddenErrors <- matVecDense whoNNT outputErrors
    -- Calculate a "gradient" with the expected training value, the
    -- NN calculated output, which is multiplied by the hidden NN outputs
        preWHO <- kerMap outputErrors finalOutputs hiddenOutputs
    -- Apply the learning rate to the newly calculated output weights
        whoDelta <- RP.computeP . RP.transpose $ RP.map (lRateNN *) preWHO :: IO NNLayerR
    -- Make the "gradient" but in this case for the input weights
        preWIH <- kerMap hiddenErrors hiddenOutputs inputs
    -- Apply the learning rate to the newly calculated input weights
        wihDelta <- RP.computeP . RP.transpose $ RP.map (lRateNN *) preWIH :: IO NNLayerR
    -- Update inner layers
        wihFin <- RP.computeP $ wihNN RP.+^ wihDelta
        whoFin <- RP.computeP $ whoNN RP.+^ whoDelta
    -- Create a new NN with updated input/output weights
        return $ NeuralNetworkR lRateNN wihFin whoFin

-- | Match query steps from the Python example
queryR :: NeuralNetworkR -> NLayerR -> IO NLayerR
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

matVecDense ::  NNLayerR -> NLayerR -> IO NLayerR
matVecDense x y = do
    let (RP.Z RP.:. s1 RP.:. s2) = RP.extent x
    extended <- RP.computeP $ RP.extend (RP.Any RP.:. s1 RP.:. RP.All) y :: IO NNLayerR
    pre <- RP.computeP $ x RP.*^ extended :: IO NNLayerR
    RP.sumP pre

matSumVecDense ::  NNLayerR -> NLayerR -> IO NNLayerR
matSumVecDense x y = do
    let (RP.Z RP.:. sy) = RP.extent y
    extended <- RP.computeP $ RP.extend (RP.Any RP.:. sy RP.:. RP.All) y :: IO NNLayerR
    RP.computeP $ x RP.+^ extended

outerProd :: NLayerR -> NLayerR -> IO NNLayerR
outerProd x y = do
    let (RP.Z RP.:. sx) = RP.extent x
    let (RP.Z RP.:. sy) = RP.extent y
    extendX <- RP.computeP $ RP.extend (RP.Any RP.:. sy RP.:. RP.All) x :: IO NNLayerR
    extendY <- RP.computeP . RP.transpose $ RP.extend (RP.Any RP.:. sx RP.:. RP.All) y :: IO NNLayerR
    RP.computeP $ extendX RP.*^ extendY -- :: IO NNLayerR

minMap :: NNT -> NLayerR -> IO NLayerR
minMap val layer = RP.computeP $ RP.map (val -) layer

kerMap :: NLayerR -> NLayerR -> NLayerR -> IO NNLayerR
kerMap outErr outs hidden = do
    minusOutputs <- minMap 1 outs
    preKer <- RP.computeP $ outErr RP.*^ outs  RP.*^ minusOutputs
    outerProd preKer hidden
