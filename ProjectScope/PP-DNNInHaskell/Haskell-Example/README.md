# NNHaskell

Simple neural network that analyzes MNIST data in CSV format.

To compile, simply clone the repository and run (within the folder):

```bash
stack build
stack exec NNHaskell-exe 5 0.01 2
```

The first is the number of hidden layers the neural network will have. The second
parameter stands for the learning rate. The third is the number of epochs to
train the NN.