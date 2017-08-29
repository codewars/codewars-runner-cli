#!/bin/bash
if nc -z 127.0.0.1 8545; then # TestRPC already running
  pkill -f "node /usr/local/bin/testrpc"
fi
testrpc &
