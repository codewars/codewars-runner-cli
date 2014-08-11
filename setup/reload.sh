#!/bin/sh

pm2 kill
pm2 start ../server.js
