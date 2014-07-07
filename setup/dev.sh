#!/bin/sh

echo "Install server packages"
cd /vagrant
npm install

echo "Install Ruby"
apt-get install -y ruby2.0

#link the ruby command to 2.0 because ubuntu is dumb
rm /usr/bin/ruby
ln /usr/bin/ruby2.0 /usr/bin/ruby

## install bundler
gem install rspec --no-ri --no-rdoc
#gem install minitest --no-ri --no-rdoc


echo "Install mono"
apt-get install -y mono-csharp-shell --fix-missing

# Install Coffeescript
npm install coffee-script -g
npm install chai -g
sudo npm install mocha -g

echo "Install supervisor"
sudo npm install supervisor -g

echo "Install Java"
# apt-get install -y default-jre-headless default-jdk # default is OpenJDK6
add-apt-repository ppa:webupd8team/java
apt-get update
# http://askubuntu.com/a/190674
echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
    echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections
apt-get install -y oracle-java8-installer

echo "Install Clojure"
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > /usr/bin/lein
chmod a+x /usr/bin/lein
# Add a few packages by default
mkdir ~/.lein && echo '{:user {:dependencies [[org.clojure/clojure "1.6.0"] [junit/junit "4.11"] [org.hamcrest/hamcrest-core "1.3"]]}}' > ~/.lein/profiles.clj
echo '(defproject codewars "Docker")' > project.clj
LEIN_ROOT=true lein deps

# Install Haskell
DEBIAN_FRONTEND=noninteractive apt-get install -y ghc cabal-install
cabal update
cabal install hspec

node build
