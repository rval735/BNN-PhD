#!/bin/bash

export LC_ALL="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
sudo dpkg-reconfigure locales

sudo apt update && sudo apt-get install build-essential git cmake -y
sudo apt-get install libatlas-base-dev -y
sudo apt-get install -y libopenblas-dev
sudo apt-get install -y libopencv-dev
sudo apt install cmake-curses-gui
sudo apt install python-pip -y
pip install --upgrade pip
pip install mxnet
pip install --user requests jupyter
pip install --user matplotlib

#-------------------------#
# Intel MKL #
# https://gist.github.com/pachamaltese/afc4faef2f191b533556f261a46b3aa8
#-------------------------#
wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS-2019.PUB | apt-key add -
sudo sh -c 'echo deb https://apt.repos.intel.com/mkl all main > /etc/apt/sources.list.d/intel-mkl.list'
sudo apt-get update && sudo apt-get install intel-mkl-64bit-2019.0-045
printf '/opt/intel/lib/intel64\n/opt/intel/mkl/lib/intel64\n' | sudo tee -a /etc/ld.so.conf.d/intel_mkl.conf
sudo ldconfig
export LD_LIBRARY_PATH=/opt/intel/lib/intel64:/opt/intel/mkl/lib/intel64:${LD_LIBRARY_PATH}

#-------------------------#
# BMXNet #
#-------------------------#

git clone --recursive https://github.com/hpi-xnor/mxnet.git # remember to include the --recursive
cd mxnet && mkdir build
mkdir build/Release && cd build/Release
cmake ../../ # if any error occurs, apply ccmake or cmake-gui to adjust the cmake config.
ccmake . # or GUI cmake
make -j `nproc`

cd .. && cd .. && cd python
pip install --upgrade pip

export LD_LIBRARY_PATH=/home/ubuntu/bmxnet/mxnet/build/Release
export PYTHONPATH=/home/ubuntu/bmxnet/mxnet/python
export PY_USER_BIN=$(python -c 'import site; print(site.USER_BASE + "/bin")')
export PATH=$PY_USER_BIN:$PATH

pip install --user .