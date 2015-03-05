#!/bin/sh

cd /codewars-runner

echo "Pulling latest source..."
git pull

echo "Installing..."
npm install

echo "Pulling latest images..."
node pull

echo "Cleaning up..."
# delete old containers that are stopped but not removed
docker rm $(docker ps -a -q)

# delete old images that are taking up space
docker rmi $(docker images | grep "^<none>" | awk "{print $3}")
