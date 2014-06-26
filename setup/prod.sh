#!/bin/sh

echo "Setting up docker group"
sudo groupadd docker
sudo gpasswd -a safeuser docker
sudo service docker restart

cd /codewars-runner

echo "Giving user permission to the directory"
sudo chown -R safeuser:safeuser .

echo "Pulling latest docker image..."
node pull

echo "Starting pm2"
pm2 --run-as-user safeuser start server.js

echo "Installing startup script"
pm2 startup ubuntu

echo "Saving pm2 configuration"
pm2 save
