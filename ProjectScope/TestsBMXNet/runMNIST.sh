#!/bin/bash

echo "About to start MNIST Test"

./run10MNISTTest.sh

echo "------------------------------------"
echo "About to start MNIST BNN"

cd bnn
./run10MNISTBNNTest.sh

echo "Finished both"

# To run this script
# nohup ./runMNIST.sh &>> results/DNN-CNN-BNN.txt &