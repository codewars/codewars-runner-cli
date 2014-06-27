#!/bin/bash

echo "make sure you run this with bash"
echo -n $((( RANDOM % 60 ))) > cronfile
echo " * * * * /sbin/restart docker" >> cronfile
crontab cronfile
rm cronfile
