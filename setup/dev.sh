#!/bin/sh

echo "INSTALL MONO SHELL"
apt-get -y install mono-csharp-shell --fix-missing

echo "Install supervisor"
npm install -g supervisor

cd /vagrant
node build
