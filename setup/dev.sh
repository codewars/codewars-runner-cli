#!/bin/sh

echo "Install server packages"
cd /vagrant
npm install

echo "Install supervisor"
sudo npm install supervisor -g

echo "Install htop"
apt-get install htop

node build
