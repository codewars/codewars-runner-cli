#!/bin/sh

pm2 kill
pm2 start /codewars-runner/server.js
