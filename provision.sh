#!/bin/sh
echo "APT-GET UPDATE"
apt-get update

apt-get install monit

# Add nodejs repo
add-apt-repository -y ppa:chris-lea/node.js
apt-get -y update

echo "INSTALL NODEJS"
apt-get install -y nodejs

echo "INSTALL NPM"
apt-get -qq -y install npm

echo "Install supervisor"
npm install -g supervisor

echo "Install server packages"
cd /vagrant
npm install

