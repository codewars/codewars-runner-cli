#!/bin/bash -x
set -euo pipefail
IFS=$'\n\t'

# Add nodejs repo
add-apt-repository -y ppa:chris-lea/node.js

apt-get -y update
apt-get install -y iptables
apt-get install -y monit

apt-get install -y nodejs
npm install pm2 -g --unsafe-perm

# Setting some Vagrant sandbox limits
iptables -A OUTPUT -m owner --uid-owner 1000 -j DROP
sed -i '$a vagrant soft nproc 20' /etc/security/limits.conf
sed -i '$a session required pam_limits.so' /etc/pam.d/common-session
