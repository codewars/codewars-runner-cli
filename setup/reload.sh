#!/bin/bash -x
set -euo pipefail
IFS=$'\n\t'

pm2 restart server
