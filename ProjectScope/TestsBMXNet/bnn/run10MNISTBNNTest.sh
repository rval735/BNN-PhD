#!/bin/bash

echoDate()
{
    echo $(date +%F_%T)
}

echo "About to start MNIST BNN"
echoDate
for i in `seq 1 10`; do
    /usr/bin/time -va python mnist_cnn.py --o trained_model --epochs 10
done

echoDate

echo "Finished"