#!/bin/bash -x
set -euo pipefail
IFS=$'\n\t'

cd /codewars-runner
git pull
npm install
node pull

# delete old containers that are stopped but not removed
docker rm $(docker ps -a -q)

# delete old images that are taking up space
docker rmi $(docker images | grep "^<none>" | awk "{print $3}")
