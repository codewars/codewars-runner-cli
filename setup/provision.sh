#!/bin/sh

echo "APT-GET UPDATE"
apt-get -y update

echo "INSTALL SysOp Tools"
apt-get install -y iptables

apt-get install monit

# Add nodejs repo
add-apt-repository -y ppa:chris-lea/node.js

echo "INSTALL NODEJS"
apt-get install -y nodejs

echo "Symlinking nodejs -> node"
sudo ln -s /usr/bin/nodejs /usr/bin/node

echo "INSTALL NPM"
apt-get -qq -y install npm

echo "Installing pm2"
sudo npm install pm2 -g


echo "Setting some sandbox limits"
sudo iptables -A OUTPUT -m owner --uid-owner 1000 -j DROP
sudo sed -i '$a vagrant soft nproc 20' /etc/security/limits.conf
sudo sed -i '$a session required pam_limits.so' /etc/pam.d/common-session
