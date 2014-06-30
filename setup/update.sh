#!/bin/sh

cd /codewars-runner
git pull
npm install
node pull
exec pm2 reload all
