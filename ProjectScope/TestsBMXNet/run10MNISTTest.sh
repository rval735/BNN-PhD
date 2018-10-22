#!/bin/bash

echoDate()
{
    echo $(date +%F_%T)
}

echo "About to start MNIST DNN"
echoDate

for i in `seq 1 10`; do
    /usr/bin/time -va python MNISTTest.py
done

echo "About to start MNIST CNN"

for i in `seq 1 10`; do
    /usr/bin/time -va python MNISTTestCNN.py
done

echoDate

echo "Finished"