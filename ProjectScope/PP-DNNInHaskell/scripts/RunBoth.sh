##
## CNN-PhD version 0.1, Copyright (C) 7/Mar/2018
## Modifier: rval735
## This code comes with ABSOLUTELY NO WARRANTY; it is provided as "is".
## This is free software under GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version. Check the LICENSE file at the root
## of this repository for more details.
##
## Description: This bash script will execute the haskell and python Neural
##  Network considering variable parameters to obtain an
## statistical sample of the execution for the time and error of it.

#!/bin/bash

SAMPLE=(100)
while [ $SAMPLE -gt 0 ]; do
    cd PythonExample
    ../scripts/RunPython.sh
    cd ..
    cd scripts
    ./RunHaskell.sh
    cd ..
    SAMPLE=$[$SAMPLE-1]
done
