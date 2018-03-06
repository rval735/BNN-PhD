----
---- CNN-PhD version 0.1, Copyright (C) 3/Mar/2018
---- Creator: rval735
---- This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
---- This is free software under GNU General Public License as published by
---- the Free Software Foundation; either version 3 of the License, or
---- (at your option) any later version. Check the LICENSE file at the root
---- of this repository for more details.
----

-- |Simple Neural Network class that has one hidden layer
module NNClass
-- (
--     randomNormal,
--     NNBase (..),
--     NeuralNetwork (..),
--     activationFunc,
--     logisticFunc,
--     matrixMult,
--     vDot,
--     train,
--     createNN,
--     updateFunc
-- )
where

import           Control.Monad
import           System.Random

-- # Numerical library in python
-- import numpy
-- # This imports the sigmoid function expit()
-- from scipy.special import expit
--
-- # Class definition for the Neural Network
-- class NeuralNetwork:
--     # NN initialization method
--     def __init__(self, inputnodes, hiddennodes, outputnodes, learningrate):
--         # set number of nodes in each input, hidden, output layer
--         self.inodes = inputnodes
--         self.hnodes = hiddennodes
--         self.onodes = outputnodes
--
--         # learning rate
--         self.lr = learningrate
--
-- |Data Structure to keep track of the NN layers
type InputNodes = Int
type OutputNodes = Int
type HiddenNodes = Int
type LearningRate = Float
type NLayer = [Float]

data NNBase = NNBase InputNodes HiddenNodes OutputNodes LearningRate

data NeuralNetwork = NeuralNetwork {
    lrate :: LearningRate,
    wih   :: [NLayer],
    who   :: [NLayer]
    } deriving (Show)

createNN :: NNBase -> IO NeuralNetwork
createNN (NNBase x y z lr) = do
    wihL <- randomNormal x y
    whoL <- randomNormal y z
    return $ NeuralNetwork lr wihL whoL

--         # link weight matrices, wih and who
--         # weights inside the arrays are w_i_j, where link is from node i to node j in the next layer
--         # w11 w21
--         # w12 w22 etc
--         self.wih = numpy.random.normal(0.0, pow(self.inodes, -0.5), (self.hnodes, self.inodes))
--         self.who = numpy.random.normal(0.0, pow(self.hnodes, -0.5), (self.onodes, self.hnodes))
--
-- |Impure function that generates random list floats, with  standard
--  deviation of x^-0.5 around 0, the size of each inner list (y) and the
--  number of list (x) with a
randomNormal :: Int -> Int -> IO [NLayer]
randomNormal y x = replicateM y $ replicateM x $ randomRIO (stdDev * (-2), stdDev * 2)
    where stdDev = fromIntegral x ** (-0.5)
--
--         # activation function is the sigmoid function
--         self.activation_function = lambda x: expit(x)
--         pass
activationFunc :: NLayer -> NLayer
activationFunc = map logisticFunc

logisticFunc :: Float -> Float
logisticFunc x = 1 / (1 + exp (-x))
--
--     # train the neural network
--     def train(self, inputs_list, targets_list):
--         # convert inputs list to 2d array
--         inputs = numpy.array(inputs_list, ndmin=2).T
--         targets = numpy.array(targets_list, ndmin=2).T
--
--         # calculate signals into hidden layer
--         hidden_inputs = numpy.dot(self.wih, inputs)
--         # calculate the signals emerging from hidden layer
--         hidden_outputs = self.activation_function(hidden_inputs)
--
--         # calculate signals into final output layer
--         final_inputs = numpy.dot(self.who, hidden_outputs)
--         # calculate the signals emerging from final output layer
--         final_outputs = self.activation_function(final_inputs)
--
--         # output layer error is the (target - actual)
--         output_errors = targets - final_outputs
--         # hidden layer error is the output_errors, split by weights, recombined at hidden nodes
--         hidden_errors = numpy.dot(self.who.T, output_errors)
--
--         # update the weights for the links between the hidden and output layers
--         self.who += self.lr * numpy.dot((output_errors * final_outputs * (1.0 - final_outputs)), numpy.transpose(hidden_outputs))
--
--         # update the weights for the links between the input and hidden layers
--         self.wih += self.lr * numpy.dot((hidden_errors * hidden_outputs * (1.0 - hidden_outputs)), numpy.transpose(inputs))
--         pass
--
train :: NLayer -> NLayer -> NeuralNetwork -> NeuralNetwork
train inputs training nn = do
    let wihNN = wih nn
    let whoNN = who nn
    let lrateNN = lrate nn
    let hiddenInputs = matrixMult wihNN inputs
    let hiddenOutputs = activationFunc hiddenInputs
    let finalInputs = matrixMult whoNN hiddenOutputs
    let finalOutputs = activationFunc hiddenInputs
    let outputErrors = zipWith (-) training finalOutputs
    let hiddenErrors = matrixMult whoNN outputErrors
    let whoDelta = updateFunc lrateNN outputErrors finalOutputs hiddenOutputs
    let wihDelta = updateFunc lrateNN hiddenErrors hiddenInputs inputs
    -- let whoDelta = lrateNN * vDot (outputErrors * finalOutputs * (1 - finalOutputs)) hiddenOutputs
    -- let wihDelta = lrateNN * vDot (hiddenErrors * hiddenInputs * (1 - hiddenOutputs)) inputs
    let wohUpdate = map (zipWith (+) whoDelta) whoNN
    let wihUpdate = map (zipWith (+) wihDelta) wihNN
    NeuralNetwork lrateNN wihUpdate wohUpdate

matrixMult :: [NLayer] -> NLayer -> NLayer
matrixMult xs y = map (vDot y) xs

vDot :: NLayer -> NLayer -> Float
vDot col row = sum $ zipWith (*) col row

updateFunc :: Float -> NLayer -> NLayer -> NLayer -> NLayer
updateFunc rate x y z = do
    let zipOut = zipWith (*) x y
    let zip1Min = map (1 - ) y
    let zipMin = zipWith (*) zipOut zip1Min
    let zipRes = zipWith (*) zipMin z
    map (rate *) zipRes


--     # query the neural network
--     def query(self, inputs_list):
--         # convert inputs list to 2d array
--         inputs = numpy.array(inputs_list, ndmin=2).T
--
--         # calculate signals into hidden layer
--         hidden_inputs = numpy.dot(self.wih, inputs)
--         # calculate the signals emerging from hidden layer
--         hidden_outputs = self.activation_function(hidden_inputs)
--
--         # calculate signals into final output layer
--         final_inputs = numpy.dot(self.who, hidden_outputs)
--         # calculate the signals emerging from final output layer
--         final_outputs = self.activation_function(final_inputs)
--
--         return final_outputs

query :: NeuralNetwork -> NLayer -> NLayer
query nn inputs = do
    let wihNN = wih nn
    let whoNN = who nn
    let lrateNN = lrate nn
    let hiddenInputs = matrixMult wihNN inputs
    let hiddenOutputs = activationFunc hiddenInputs
    let finalInputs = matrixMult whoNN hiddenOutputs
    activationFunc finalInputs
