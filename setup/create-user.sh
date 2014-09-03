#!/bin/bash -x
set -euo pipefail
IFS=$'\n\t'
useradd -s /bin/bash -m -d /home/safeuser -c "safe user" safeuser
usermod -aG safeuser
chown -R safeuser:safeuser /codewars-runner
