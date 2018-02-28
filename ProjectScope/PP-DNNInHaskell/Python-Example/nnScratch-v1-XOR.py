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

def main():
    topology = [2,3,1]
    net = Network(topology)
    Neuron.eta = 0.01
    Neuron.alpha = 0.05
    inputs = [[0, 0], [0, 1], [1, 0], [1, 1]]
    # Trouble with the NN module, if we
    # change the structure of the output, it
    # will not run due to "array mismatch",
    # because with XOR we only need one output
    # and not a pair as needed for the half
    # adder example.
    outputs = [[0, 0], [1, 0], [1, 0], [0, 1]]
    while True:
        err = 0
        for i in range(len(inputs)):
            net.setInput(inputs[i])
            net.feedForward()
            net.backPropagate(outputs[i])
            err = err + net.getError(outputs[i])
        print "error: ", err
        if err < 2.5:
            break

    while True:
        # Trouble with python, if the input
        # is not sanitized, it breaks at runtime
        a = input("type 1st input :")
        b = input("type 2nd input :")
        net.setInput([a, b])
        net.feedForward()
        print net.getThResults()

if __name__ == '__main__':
    main()