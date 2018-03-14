----
---- CNN-PhD version 0.1, Copyright (C) 3/Mar/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

{-# LANGUAGE BangPatterns #-}

-- | Simple Neural Network class that has one hidden layer
module NNClass where

import           Numeric.LinearAlgebra         (outer, scale)
import           Numeric.LinearAlgebra.Devel   (mapMatrixWithIndex,
                                                mapVectorWithIndex)
import           Numeric.LinearAlgebra.HMatrix (Matrix, R, Vector, randn, tr',
                                                ( #> ), (<.>))

-- | Code synonyms to ease relationship between function
--   parameters and their application
type InputNodes = Int
type OutputNodes = Int
type HiddenNodes = Int
type LearningRate = R
type Epochs = Int
type NLayer = Vector R
type NNLayer = Matrix R

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
    lrate :: LearningRate,
    wih   :: !NNLayer,
    who   :: !NNLayer
    } deriving (Show)

-- | Take a NNBase, then create a NeuralNetwork from its parameters
--   Impure function becase it uses random numbers
createNN :: NNBase -> IO NeuralNetwork
createNN (NNBase x y z lr) = do
    wihL <- randomNormal x y
    whoL <- randomNormal y z
    return $ NeuralNetwork lr wihL whoL

-- | Impure function that generates a normalized random matrix of doubles
--   considering input - hidden layers
randomNormal :: InputNodes -> OutputNodes -> IO (Matrix R)
randomNormal inode hnode = randn hnode inode

-- | Vector application of the "Logistic" activation function
activationFunc :: NLayer -> NLayer
activationFunc = mapVectorWithIndex (\_ v -> logisticFunc v)

-- | Logistic function formula, taking R and returning R
logisticFunc :: R -> R
logisticFunc x = 1 / (1 + exp (-x))

-- | Match training steps from the Python example
train :: NLayer -> NLayer -> NeuralNetwork -> NeuralNetwork
train inputs training (NeuralNetwork lRateNN wihNN whoNN) =
    -- Create a new NN with updated input/output weights
    NeuralNetwork lRateNN (wihNN + wihDelta) (whoNN + whoDelta)
    where
    -- Multiply training inputs against input weights
        hiddenInputs =  wihNN #> inputs
    -- Run the activation function from the result
        hiddenOutputs = activationFunc hiddenInputs
    -- Multiply activated training inputs against output weights
        finalInputs = whoNN #> hiddenOutputs
    -- Run the activation function from the output weights result
        finalOutputs = activationFunc finalInputs
    -- Match the NN prediction agains the expected training value
        outputErrors = training - finalOutputs
    -- Multiply the difference error with the NN output weights
        hiddenErrors = tr' whoNN #> outputErrors
    -- Calculate a "gradient" with the expected training value, the
    -- NN calculated output, which is multiplied by the hidden NN outputs
        preWHO = outer (outputErrors * finalOutputs * (1.0 - finalOutputs)) hiddenOutputs
    -- Apply the learning rate to the newly calculated output weights
        whoDelta = scale lRateNN preWHO
    -- Make the "gradient" but in this case for the input weights
        preWIH = outer (hiddenErrors * hiddenOutputs * (1.0 - hiddenOutputs)) inputs
    -- Apply the learning rate to the newly calculated input weights
        wihDelta = scale lRateNN preWIH

-- | Match query steps from the Python example
query :: NeuralNetwork -> NLayer -> NLayer
query (NeuralNetwork lRateNN wihNN whoNN) inputs =
    activationFunc finalInputs
    where
    -- Multiply training inputs against input weights
        hiddenInputs = wihNN #> inputs
    -- Run the activation function from the result
        hiddenOutputs = activationFunc hiddenInputs
    -- Multiply activated training inputs against output weights
        finalInputs = whoNN #> hiddenOutputs
