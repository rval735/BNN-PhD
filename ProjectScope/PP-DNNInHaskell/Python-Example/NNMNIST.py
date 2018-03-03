##
## CNN-PhD version 0.1, Copyright (C) 3/Mar/2018
## Modifier: rval735
## This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
## This is free software under GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version. Check the LICENSE file at the root
## of this repository for more details.
##
## Original code can be found here:
## https://github.com/makeyourownneuralnetwork/makeyourownneuralnetwork/blob/master/part2_neural_network_mnist_data.ipynb
## Based in ideas from this ebook:
## https://www.amazon.co.uk/Make-Your-Own-Neural-Network/dp/1530826608
## Later modified to address different objectives.

# Numerical library in python
import numpy
# Have functions for system call
import sys
# Include NN-Class for its methods
from NNClass import  *

args = sys.argv

# We expect at least 4 elements in the program arguments
# which would count for hidden nodes, learning rate, and epochs.
if len(args) < 4:
    print "This program needs 3 inputs in the following order:"
    print "Hidden nodes (integer)"
    print "Learning rate (float)"
    print "Number of Epochs (integer)"
    sys.exit()

# Assign the hidden nodes in the inner NN layer
hiddenNodes = int(args[1])

# Check that the value passed is a valid integer
if isinstance(hiddenNodes, int) == False:
    print "This program needs input 1 for hidden nodes to be integer"
    sys.exit()

# Determine how big/small steps are made after an epoch
learningRate = float(args[2])
# Check that the value passed is a valid float
if isinstance(learningRate, float) == False:
    print "This program needs input 2 for learning rate to be float"
    sys.exit()

# Number of times the training data set is iterated to train the NN
epochs = int(args[3])
# Check that the value passed is a valid integer
if isinstance(epochs, int) == False:
    print "This program needs inputs 3 for epochs to be integer"
    sys.exit()

# Number of inputs, considering a 784 size for the MNIST dataset
inputNodes = 784
# Number output nodes, which is 10 for numbers 0 to 9
outputNodes = 10
# create instance of neural network
n = NeuralNetwork(inputNodes, hiddenNodes, outputNodes, learningRate)

# Load MNIST dataset in CSV format into a list
training_data_file = open("../MNIST-Data/MNIST-Train.csv", 'r')
training_data_list = training_data_file.readlines()
training_data_file.close()

# train the neural network
for e in range(epochs):
    print "Epoch: ", e
    # go through all records in the training data set
    for record in training_data_list:
        # split the record by the ',' commas
        all_values = record.split(',')
        # scale and shift the inputs
        inputs = (numpy.asfarray(all_values[1:]) / 255.0 * 0.99) + 0.01
        # create the target output values (all 0.01, except the desired label which is 0.99)
        targets = numpy.zeros(outputNodes) + 0.01
        # all_values[0] is the target label for this record
        targets[int(all_values[0])] = 0.99
        n.train(inputs, targets)
        pass
    pass

# load the mnist test data CSV file into a list
test_data_file = open("../MNIST-Data/MNIST-Train-100.csv", 'r')
test_data_list = test_data_file.readlines()
test_data_file.close()

# test the neural network

# scorecard for how well the network performs, initially empty
scorecard = []

# go through all the records in the test data set
for record in test_data_list:
    # split the record by the ',' commas
    all_values = record.split(',')
    # correct answer is first value
    correct_label = int(all_values[0])
    # scale and shift the inputs
    inputs = (numpy.asfarray(all_values[1:]) / 255.0 * 0.99) + 0.01
    # query the network
    outputs = n.query(inputs)
    # the index of the highest value corresponds to the label
    label = numpy.argmax(outputs)
    # append correct or incorrect to list
    if (label == correct_label):
        # network's answer matches correct answer, add 1 to scorecard
        scorecard.append(1)
    else:
        # network's answer doesn't match correct answer, add 0 to scorecard
        scorecard.append(0)
        pass
    
    pass

# calculate the performance score, the fraction of correct answers
scorecard_array = numpy.asarray(scorecard)
print ("SC = ", scorecard_array.sum())
print ("Size = ", scorecard_array.size)
print ("performance = ", scorecard_array.sum() / float(scorecard_array.size))