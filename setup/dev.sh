#!/bin/sh

echo "INSTALL MONO SHELL"
apt-get -y install mono-csharp-shell --fix-missing

cd /vagrant

echo "Install supervisor"
sudo npm install supervisor -g

node build
