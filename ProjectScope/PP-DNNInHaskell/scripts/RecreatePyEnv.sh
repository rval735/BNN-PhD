##
## CNN-PhD version 0.1, Copyright (C) 15/Mar/2018
## Modifier: rval735
## This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
## This is free software under GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version. Check the LICENSE file at the root
## of this repository for more details.
##
## Description: This bash script will recreate a python environment to
## run the code from the "cnn-phd" python neural network git project:
## https://github.com/rval735/cnn-phd/tree/master/ProjectScope/PP-DNNInHaskell

#!/bin/bash

sudo apt -y update
sudo apt -y upgrade
sudo apt -y install build-essential libssl-dev libffi-dev python-dev
sudo apt -y install python3-pip
sudo pip3 install --upgrade pip
sudo pip3 install --upgrade virtualenv
sudo pip3 install numpy
sudo pip3 install scipy
sudo apt-get install libgsl0-dev liblapack-dev libatlas-base-dev

cd ~/
git clone https://github.com/rval735/cnn-phd.git
cd cnn-phd/ProjectScope/PP-DNNInHaskell/MNIST-Data/
wget --no-check-certificate https://pjreddie.com/media/files/mnist_train.csv
wget --no-check-certificate https://pjreddie.com/media/files/mnist_test.csv
mv mnist_test.csv MNIST-Test.csv
mv mnist_train.csv MNIST-Train.csv

cd ..
cd Python-Example/
time ../scripts/RunPython.sh
