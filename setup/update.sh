#!/bin/sh

cd /codewars-runner
git pull
npm install
node pull
docker ps -a | grep 'ago' | awk '{print $1}' | xargs docker rm
