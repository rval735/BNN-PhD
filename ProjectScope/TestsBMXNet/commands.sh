#!/bin/bash

export LC_ALL="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
sudo dpkg-reconfigure locales

sudo apt install python-pip
pip install --upgrade pip
pip install mxnet
pip install --user requests jupyter