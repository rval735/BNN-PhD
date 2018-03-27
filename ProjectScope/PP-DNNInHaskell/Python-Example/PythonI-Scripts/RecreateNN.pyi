import numpy
from scipy.special import expit
# inputNodes = 784
inputNodes = 4
outputNodes = 3
hiddenNodes = 2
lrate = 0.01
epochs = 1
        
# wih = numpy.random.normal(0.0, pow(inputNodes, -0.5), (hiddenNodes, inputNodes))
wih = numpy.array([[i for i in range(4)], [i for i in range(4,8)]])
# who = numpy.random.normal(0.0, pow(hiddenNodes, -0.5), (outputNodes, hiddenNodes))
who = numpy.array([[i for i in range(2)], [i for i in range(2,4)], [i for i in range(4,6)]])

activationF = lambda x: expit(x)

inputs_list = numpy.array([i for i in range(inputNodes)])
targets_list = numpy.array([i for i in range(outputNodes)])
        
inputs = numpy.array(inputs_list, ndmin=2).T
targets = numpy.array(targets_list, ndmin=2).T
hidden_inputs = numpy.dot(wih, inputs)
hidden_outputs = activationF(hidden_inputs)
final_inputs = numpy.dot(who, hidden_outputs)
final_outputs = activationF(final_inputs)
output_errors = targets - final_outputs
hidden_errors = numpy.dot(who.T, output_errors)
preWHO = numpy.dot((output_errors * final_outputs * (1.0 - final_outputs)), numpy.transpose(hidden_outputs))
preWIH = numpy.dot((hidden_errors * hidden_outputs * (1.0 - hidden_outputs)), numpy.transpose(inputs))
whoDelta = lrate * preWHO
wihDelta = lrate * preWIH
whoFin = whoDelta + who
wihFin = wihDelta + wih