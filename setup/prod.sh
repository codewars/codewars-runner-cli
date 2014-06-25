#!/bin/sh

sudo npm install pm2 -g

cd /
git clone https://github.com/codewars/codewars-runner
sudo cp /codewars-runner/setup/codewars-runner.conf /etc/init/codewars-runner.conf

cd /codewars-runner
node pull
pm2 start server
