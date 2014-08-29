#!/bin/bash -x
set -euo pipefail
IFS=$'\n\t'

cd /vagrant
npm install
npm install supervisor -g
apt-get -y install docker.io
ln -sf /usr/bin/docker.io /usr/local/bin/docker
sed -i '$acomplete -F _docker docker' /etc/bash_completion.d/docker.io
update-rc.d docker.io defaults
usermod -a -G docker vagrant
apt-get install htop

su vagrant -c "node build base"
su vagrant -c "node build"
