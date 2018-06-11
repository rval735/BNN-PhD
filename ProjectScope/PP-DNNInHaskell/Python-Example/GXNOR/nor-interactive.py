from __future__ import print_function
import sys
import os
import time
import numpy as np
np.random.seed(1234)  # for reproducibility
import theano
import theano.tensor as T
import lasagne
import cPickle as pickle
import gzip
from pylearn2.datasets.mnist import MNIST
from pylearn2.utils import serial
from collections import OrderedDict
import time
import numpy as np
from theano.ifelse import ifelse
import scipy.io as scio
from numpy import random
from numpy import multiply
from theano.sandbox.rng_mrg import MRG_RandomStreams as RandomStreams
from theano.scalar.basic import UnaryScalarOp, same_out_nocomplex
from theano.tensor.elemwise import Elemwise
from itertools import izip
from nor import *
alpha = 0.1
epsilon = 1e-4
batch_size = 10000
num_epochs = 4000
activation = discrete_neuron_3states #activation discretization
discrete = True
global update_type,best_params,H,N,th
H = 1. # the weight is in [-H, H]
N = 1. # the state number of the discrete weight space is 2^N+1
th = 3.   #the nonlinearity parameter of state transfer probability
LR_start = 0.1
LR_fin = 0.0000001
LR_decay = (LR_fin/LR_start)**(1./(num_epochs))
train_set = MNIST(which_set= 'train', start=0, stop = 50000, center = False)
valid_set = MNIST(which_set= 'train', start=50000, stop = 60000, center = False)
test_set = MNIST(which_set= 'test', center = False)
train_set.X = 2*train_set.X.reshape(-1, 1, 28, 28)-1.
valid_set.X = 2*valid_set.X.reshape(-1, 1, 28, 28)-1.
test_set.X = 2*test_set.X.reshape(-1, 1, 28, 28)-1.
train_set.y = np.hstack(train_set.y)
valid_set.y = np.hstack(valid_set.y)
test_set.y = np.hstack(test_set.y)
train_set.y = np.float32(np.eye(10)[train_set.y])
valid_set.y = np.float32(np.eye(10)[valid_set.y])
test_set.y = np.float32(np.eye(10)[test_set.y])
train_set.y = 2* train_set.y - 1.
valid_set.y = 2* valid_set.y - 1.
test_set.y = 2* test_set.y - 1.
input = T.tensor4('inputs')
target = T.matrix('targets')
LR = T.scalar('LR', dtype=theano.config.floatX)
update_type = 200

cnn = lasagne.layers.InputLayer(shape=(None, 1, 28, 28),input_var=input)

cnn = Conv2DLayer(cnn,discrete=discrete,H=H,N=N,num_filters=32,filter_size=(5, 5),pad = 'valid',nonlinearity=lasagne.nonlinearities.identity)

cnn = lasagne.layers.MaxPool2DLayer(cnn, pool_size=(2, 2))

cnn = lasagne.layers.BatchNormLayer(cnn,epsilon=epsilon,alpha=alpha)
            
cnn = lasagne.layers.NonlinearityLayer(cnn,nonlinearity=activation)
        
cnn = Conv2DLayer(cnn,discrete=discrete,H=H,N=N,num_filters=64,filter_size=(5, 5),pad = 'valid',nonlinearity=lasagne.nonlinearities.identity)

cnn = lasagne.layers.MaxPool2DLayer(cnn, pool_size=(2, 2))

cnn = lasagne.layers.BatchNormLayer(cnn,epsilon=epsilon,alpha=alpha)
            
cnn = lasagne.layers.NonlinearityLayer(cnn,nonlinearity=activation)

cnn = DenseLayer(cnn,discrete=discrete,H=H,N=N,num_units=512,nonlinearity=lasagne.nonlinearities.identity)

cnn = lasagne.layers.BatchNormLayer(cnn,epsilon=epsilon,alpha=alpha)

cnn = lasagne.layers.NonlinearityLayer(cnn,nonlinearity=activation)

cnn = DenseLayer(cnn,discrete=discrete,H=H,N=N,num_units=10,nonlinearity=lasagne.nonlinearities.identity)

cnn = lasagne.layers.BatchNormLayer(cnn,epsilon=epsilon,alpha=alpha)

train_output = lasagne.layers.get_output(cnn, deterministic=False)
best_params = lasagne.layers.get_all_params(cnn, discrete=True)

loss = T.mean(T.sqr(T.maximum(0.,1.-target*train_output)))

updates = discrete_grads(loss,cnn,LR,update_type, best_params, H, N, th)
params = lasagne.layers.get_all_params(cnn, trainable=True, discrete=False)
updates = OrderedDict(updates.items() + lasagne.updates.adam(loss_or_grads=loss, params=params, learning_rate=LR).items())

test_output = lasagne.layers.get_output(cnn, deterministic=True)
test_loss = T.mean(T.sqr(T.maximum(0.,1.-target*test_output)))
test_err = T.mean(T.neq(T.argmax(test_output, axis=1), T.argmax(target, axis=1)),dtype=theano.config.floatX)

train_fn = theano.function([input, target, LR], loss, updates=updates)
val_fn = theano.function([input, target], [test_loss, test_err])

train(cnn,train_fn,val_fn,batch_size,LR_start,LR_decay,num_epochs,train_set.X,train_set.y,valid_set.X,valid_set.y,test_set.X,test_set.y)

