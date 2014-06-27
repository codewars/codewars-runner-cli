#!/bin/sh

echo "Install server packages"
cd /vagrant
npm install

echo "INSTALL MONO SHELL"
apt-get -y install mono-csharp-shell --fix-missing

echo "Install supervisor"
sudo npm install supervisor -g

node build
