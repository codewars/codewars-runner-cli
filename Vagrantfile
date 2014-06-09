
# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

# Inline shell script to provision Vagrant
provision_script = <<-EOF
echo "APT-GET UPDATE"
apt-get update

# Add nodejs repo
add-apt-repository -y ppa:chris-lea/node.js
apt-get -y update

echo "INSTALL NODEJS"
apt-get install -y nodejs

echo "INSTALL NPM"
apt-get -qq -y install npm

npm install -g supervisor
EOF

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "https://cloud-images.ubuntu.com/vagrant/trusty/current/trusty-server-cloudimg-amd64-vagrant-disk1.box" # 64 bit

  config.vm.provider "virtualbox" do |v|
    v.name = "codewars_runner_host"
  end

  config.vm.provision "docker" do |d|
    d.build_image "/vagrant", args: "-t codewars/cli-runner"
  end

  # Setting up a static network on 10.100.150.0 class C subnet
  # This is to make future expansion of the vagrant file to a small test cluster easier
  config.vm.network "private_network", ip: "10.100.150.2"
  config.vm.network "forwarded_port", guest: 8080, host: 8080


  config.vm.provision "shell", inline: provision_script
end