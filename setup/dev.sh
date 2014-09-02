#!/bin/sh

echo "Install server packages"
cd /vagrant
npm install

echo "Install supervisor"
npm install supervisor -g

echo "Install docker"
apt-get -y install docker
ln -sf /usr/bin/docker.io /usr/local/bin/docker
sed -i '$acomplete -F _docker docker' /etc/bash_completion.d/docker.io
echo "Setup docker to start at boot"
update-rc.d docker.io defaults
echo "Add vagrant as a dockerer"
usermod -a -G docker vagrant

echo "Install htop"
apt-get install htop

#node build
