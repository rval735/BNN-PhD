#!/bin/bash

FILE="links.txt"
while read -r LINE
do
    ./BibFromArXiv.sh $LINE
done < "$FILE"