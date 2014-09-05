#!/bin/bash -x
set -euo pipefail
IFS=$'\n\t'

echo "Cleaning up..."

# delete old containers that are stopped but not removed
docker rm $(docker ps -a -q)

# delete old images that are taking up space.
timeout 45 docker rmi $(docker images | grep "0.1." | awk "{print $3}")
timeout 15 docker rmi $(docker images | grep "^<none>" | awk "{print $3}")


