#!/bin/bash

echo "About to start MNIST Test"

./run10MNISTTest.sh

echo "About to start MNIST BNN"

cd bnn
./run10MNISTBNNTest.sh

echo "Finished both"

# To run this script
# nohup ./run10MNISTTest.sh &>> results/allTest0.txt &