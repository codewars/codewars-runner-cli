#!/bin/sh

sudo npm install pm2 -g

cd /
sudo cp /codewars-runner/setup/codewars-runner.conf /etc/init/codewars-runner.conf

cd /codewars-runner

echo "Pulling latest docker image..."
node pull
pm2 --run-as-user safeuser start server.js
