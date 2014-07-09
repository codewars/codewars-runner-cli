#!/bin/sh

cd /codewars-runner
pm2 kill
pm2 start server.js
