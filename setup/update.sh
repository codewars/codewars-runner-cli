#!/bin/sh

cd /codewars-runner
git pull
npm install
node pull
pm2 kill
pm2 start server.js
