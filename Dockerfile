# BUILD-USING:    docker build -t codewars/cli-runner .
# TEST-USING:     docker run --rm -i -t --name=test-cli-runner --entrypoint=/bin/bash codewars/cli-runner -s
# RUN-USING:      docker run --rm --name=cli-runner codewars/cli-runner --help
# EXAMPLE USAGE:  docker run --rm codewars/cli-runner run -l ruby -c "puts 1+1"

# Pull base image.
FROM dockerfile/ubuntu

# Set the env variables to non-interactive
ENV DEBIAN_FRONTEND noninteractive
ENV DEBIAN_PRIORITY critical
ENV DEBCONF_NOWARNINGS yes
ENV TERM linux
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections

# ADD codewarrior user
RUN useradd codewarrior
RUN rm -rf ~codewarrior && cp -a ~root ~codewarrior && chown -R codewarrior:codewarrior ~codewarrior
RUN apt-get install -y python python-dev python-pip python-virtualenv

# Define mountable directories.

# Install Node.js
RUN add-apt-repository ppa:chris-lea/node.js
RUN apt-get update 
RUN apt-get install -y nodejs
RUN su codewarrior -c "echo '\n# Node.js\nexport PATH=\"/codewars/node_modules/.bin:$PATH\"' >> ~codewarrior/.bash_profile"

# Define default command.
CMD ["bash"]

# Append any relevant run args as per the help

RUN apt-get update

# Install Mono
RUN apt-get install -y mono-csharp-shell --fix-missing

# Install F#
RUN apt-get install -y fsharp

# Install Coffeescript
RUN npm -g install coffee-script

# Install Node testing frameworks & additional frameworks
RUN npm -g install chai mocha immutable

# Install Lua
RUN apt-get install -y lua5.2

# Install Python 3

# Install Additional Python libraries
RUN sudo apt-get install -y python-numpy python-scipy python-pandas

# Install Java 8
# RUN apt-get install -y default-jre-headless default-jdk # default is OpenJDK6
RUN add-apt-repository ppa:webupd8team/java 
RUN apt-get update
# http://askubuntu.com/a/190674
RUN echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
    echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections
RUN apt-get install -y oracle-java8-installer

# Install Clojure (well, install Leiningen)
RUN curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > /usr/bin/lein
RUN chmod a+x /usr/bin/lein

# Install Haskell
RUN apt-get install -y ghc cabal-install
RUN su codewarrior -c "cabal update"
RUN su codewarrior -c "cd ~codewarrior ; cabal install hspec"

# Install Julia
# Julia is really slow, but v0.3 is okay (see http://stackoverflow.com/a/20566032)
# In the future, don't use nightly builds, use releases
RUN add-apt-repository ppa:staticfloat/julianightlies
RUN add-apt-repository ppa:staticfloat/julia-deps
RUN apt-get update
RUN apt-get -y install julia
# Nightly builds have a noisy OpenBLAS error, workaround
RUN mv /usr/bin/julia /usr/bin/julia-noisy
RUN printf '#!/bin/bash\njulia-noisy "$@" 2> >(grep -v "OpenBLAS : Your OS does not support AVX instructions." 1>&2)' > /usr/bin/julia
RUN chmod a+x /usr/bin/julia

# Install erlang
RUN echo "deb http://packages.erlang-solutions.com/ubuntu trusty contrib" >> /etc/apt/sources.list
RUN curl http://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc | apt-key add -
RUN apt-get update
RUN apt-get -y install erlang-nox erlang-dev

# Install PHP
RUN apt-get -y install php5-cli

# Install GoLang
WORKDIR /tmp
# http://blog.labix.org/2013/06/15/in-flight-deb-packages-of-go
# This was cool but then it stopped working... that sucks... ~Matt
RUN curl https://godeb.s3.amazonaws.com/godeb-amd64.tar.gz | tar zxv
RUN ./godeb install 1.3.1
RUN rm godeb
#RUN apt-get install -y golang

# Install TypeScript
RUN npm -g install typescript

#Install ruby
RUN apt-get install -y python-software-properties && \
    apt-add-repository -y ppa:brightbox/ruby-ng && \
    apt-get update && \
    apt-get install -y ruby2.1 ruby2.1-dev && \
    update-alternatives --remove ruby /usr/bin/ruby2.1 && \
    update-alternatives --remove irb /usr/bin/irb2.1 && \
    update-alternatives --remove gem /usr/bin/gem2.1 && \
    update-alternatives \
        --install /usr/bin/ruby ruby /usr/bin/ruby2.1 50 \
        --slave /usr/bin/irb irb /usr/bin/irb2.1 \
        --slave /usr/bin/rake rake /usr/bin/rake2.1 \
        --slave /usr/bin/gem gem /usr/bin/gem2.1 \
        --slave /usr/bin/rdoc rdoc /usr/bin/rdoc2.1 \
        --slave /usr/bin/testrb testrb /usr/bin/testrb2.1 \
        --slave /usr/bin/erb erb /usr/bin/erb2.1 \
        --slave /usr/bin/ri ri /usr/bin/ri2.1 && \
    update-alternatives --config ruby && \
    update-alternatives --display ruby

## install bundler
RUN gem install rspec --no-ri --no-rdoc
RUN gem install rspec-its --no-ri --no-rdoc

#RUN gem install minitest --no-ri --no-rdoc

# Install additional gems
RUN gem install rails --no-ri --no-rdoc

# Install Racket
RUN apt-get -y install racket

# Install SBCL (Steel Bank Common Lisp)
RUN apt-get -y install sbcl

# Install Tiny C Compiler
RUN apt-get -y install tcc

# Install CLANG 3.4
RUN add-apt-repository ppa:ubuntu-toolchain-r/ppa
RUN apt-get update
RUN apt-get -y install clang-3.4 lldb-3.4

# Install OCAML
RUN apt-get -y install ocaml-nox

# Install Rust 0.11
# Workaround since the author of this PPA is Scandanavian, with a unicode name, and the tools hate this
RUN add-apt-repository "deb http://ppa.launchpad.net/hansjorg/rust/ubuntu trusty main"
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys BD6B6386
RUN apt-get update
RUN apt-get -y install rust-0.11

# Install SQLITE
RUN apt-get install -y sqlite libsqlite3-dev
RUN gem install sqlite3 --no-ri --no-rdoc
RUN npm -g install sqlite3

# Install MongoDB
RUN apt-get install -y mongodb-server && \
    mkdir -p /data/db && \
    chown codewarrior:codewarrior /data/db

# Install mongo packages for languages
RUN npm -g install mongoose mongodb
RUN pip install pymongo
RUN gem install mongo --no-ri --no-rdoc
RUN gem install mongoid --no-ri --no-rdoc

# Install Redis
RUN apt-get install -y redis-server

# Install Redis Language packages
RUN npm -g install redis
RUN gem install redis --no-ri --no-rdoc
RUN pip install redis

# CUDA 6
#RUN wget http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1204/x86_64/cuda-repo-ubuntu1204_6.0-37_amd64.deb && \
#    dpkg -i cuda-repo-ubuntu1204_6.0-37_amd64.deb && \
#    rm cuda-repo-ubuntu1204_6.0-37_amd64.deb
#RUN apt-get update
#RUN apt-get install -y cuda-core-6-0
# Install GPUOcelot
#RUN apt-get -y install subversion
#RUN svn checkout http://gpuocelot.googlecode.com/svn/trunk/ gpuocelot-read-only 
#RUN apt-get -y install flex bison scons libboost-all-dev llvm llvm-dev freeglut3-dev libglew-dev 
#RUN cd  gpuocelot-read-only && \
#    ./build.py --install

# NASM
RUN apt-get -y install nasm

# ARM Assembly Emulation
RUN apt-get -y install gcc-4.7-arm-linux-gnueabi libc6-dev-armel-cross qemu-user

# Install Persistent Database support for Haskell
RUN apt-get -y install libghc-zlib-dev && su codewarrior -c "cabal install esqueleto persistent-sqlite persistent-template"

# ADD cli-runner and install node deps
ADD . /codewars

# Build the jvm-runner
WORKDIR /codewars/jvm-runner
RUN [ -e target/jvm-runner-0.1.1-standalone.jar ] || LEIN_ROOT=true lein do clean, test, uberjar

WORKDIR /codewars
RUN npm install

# Run the test suite to make sure this thing works

USER codewarrior
# Set environment variables
ENV TIMEOUT 2000
ENV USER codewarrior
ENV HOME /home/codewarrior
RUN mocha -t 5000 test/*

#timeout is a fallback in case an error with node
#prevents it from exiting properly
ENTRYPOINT ["timeout", "15", "node"]
