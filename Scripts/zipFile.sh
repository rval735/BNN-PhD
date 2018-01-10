#!/bin/bash

echo $@
MD5HASH=$(md5 -q $@ | awk '{print $1}' | md5)
echo $MD5HASH
zip -rj -P $MD5HASH Project.zip $@