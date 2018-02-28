##
## CNN-PhD version 0.1, Copyright (C) 28/Feb/2018
## Creator: rval735
## This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
## This is free software under GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version. Check the LICENSE file at the root
## of this repository for more details.
##

from nnScratchOriginal import *
import numpy

def main():
    inputNodes = 784
    hiddenLayers = 200
    outputNodes = 10
    epochs = 5
    
    topology = [inputNodes, hiddenLayers, outputNodes]
    net = Network(topology)
    Neuron.eta = 0.1
    Neuron.alpha = 0.05
    
    dataFile = open("../MNIST-Data/mnist_train_100.csv", 'r')
    trainListCVS = dataFile.readlines()
    dataFile.close()
    
    for e in range(epochs):
    # go through all records in the training data set
        err = 0
        for record in trainListCVS:
            # split the record by the ',' commas
            allValues = record.split(',')
            # scale and shift the inputs
            inputs = (numpy.asfarray(allValues[1:]) / 255.0 * 0.99) + 0.01
            # create the target output values (all 0.01, except the desired label which is 0.99)
            targets = numpy.zeros(outputNodes) + 0.01
            # all_values[0] is the target label for this record
            targets[int(allValues[0])] = 0.99
            # n.train(inputs, targets)
            net.setInput(inputs)
            net.feedForward()
            net.backPropagate(targets)
            err = err + net.getError(targets)
            pass
        print "error: ", err
    pass
    #
    #
    # while epochs > 0:
    #     err = 0
    #     for i in range(len(inputs)):
    #         net.setInput(inputs[i])
    #         net.feedForward()
    #         net.backPropagate(outputs[i])
    #         err = err + net.getError(outputs[i])
    #     print "error: ", err
    #     epochs = epochs - 1
    #
    #
    dataFile = open("../MNIST-Data/mnist_train_10.csv", 'r')
    validateListCVS = dataFile.readlines()
    dataFile.close()
    
    for record in validateListCVS:
        # split the record by the ',' commas
        allValues = record.split(',')
        # scale and shift the inputs
        inputs = (numpy.asfarray(allValues[1:]) / 255.0 * 0.99) + 0.01
        # create the target output values (all 0.01, except the desired label which is 0.99)
        targets = numpy.zeros(outputNodes) + 0.01
        # all_values[0] is the target label for this record
        targets[int(allValues[0])] = 0.99
        # n.train(inputs, targets)
        net.setInput(inputs)
        net.feedForward()
        print "NN output: ", net.getResults()
        print "Expected: ", targets
        pass
    
    
    # while True:
    #     a = input("type 1st input :")
    #     b = input("type 2nd input :")
    #     net.setInput([a, b])
    #     net.feedForward()
    #     print net.getThResults()


if __name__ == '__main__':
    main()