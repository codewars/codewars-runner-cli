#!/bin/sh

cd /codewars-runner
sudo git pull
npm install
node pull
pm2 restart all
