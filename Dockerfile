# BUILD-USING:    docker build -t codewars/cli-runner .
# TEST-USING:     docker run --rm -i -t --name=test-cli-runner --entrypoint=/bin/bash codewars/cli-runner -s
# RUN-USING:      docker run --rm --name=cli-runner codewars/cli-runner --help
# EXAMPLE USAGE:  docker run --rm codewars/cli-runner run -l ruby -c "puts 1+1"

# Pull base image.
FROM dockerfile/ubuntu
RUN apt-get install -y python python-dev python-pip python-virtualenv

# Define mountable directories.

# Install Node.js
RUN \
  cd /tmp && \
  wget http://nodejs.org/dist/node-latest.tar.gz && \
  tar xvzf node-latest.tar.gz && \
  rm -f node-latest.tar.gz && \
  cd node-v* && \
  ./configure && \
  CXX="g++ -Wno-unused-local-typedefs" make && \
  CXX="g++ -Wno-unused-local-typedefs" make install && \
  cd /tmp && \
  rm -rf /tmp/node-v* && \
  echo '\n# Node.js\nexport PATH="node_modules/.bin:$PATH"' >> /root/.bash_profile


# Define default command.
CMD ["bash"]

# Append any relevant run args as per the help

RUN apt-get update

#Install ruby
RUN apt-get install -y ruby2.0

#link the ruby command to 2.0 because ubuntu is dumb
RUN rm /usr/bin/ruby
RUN ln /usr/bin/ruby2.0 /usr/bin/ruby

## install bundler
RUN gem install rspec --no-ri --no-rdoc
#RUN gem install minitest --no-ri --no-rdoc

# Install Mono
RUN apt-get install -y mono-csharp-shell --fix-missing

# Install Coffeescript
RUN npm -g install coffee-script
RUN npm -g install chai
RUN npm -g install mocha

# Install Python 3

# Install Java 8
# RUN apt-get install -y default-jre-headless default-jdk # default is OpenJDK6
RUN add-apt-repository ppa:webupd8team/java 
RUN apt-get update
# http://askubuntu.com/a/190674
RUN echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
    echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections
RUN apt-get install -y oracle-java8-installer

# Install Clojure
RUN curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > /usr/bin/lein
RUN chmod a+x /usr/bin/lein
# Add a few packages by default
RUN mkdir ~/.lein && echo '{:user {:dependencies [[org.clojure/clojure "1.6.0"] [junit/junit "4.11"] [org.hamcrest/hamcrest-core "1.3"]]}}' > ~/.lein/profiles.clj
RUN echo '(defproject codewars "Docker")' > project.clj 
RUN LEIN_ROOT=true lein deps

# Install Haskell
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y ghc cabal-install
RUN cabal update
RUN cabal install hspec

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
RUN apt-get -y install erlang

# ADD cli-runner and install node deps
ADD . /codewars
WORKDIR /codewars
RUN npm install

#timeout is a fallback in case an error with node
#prevents it from exiting properly
ENTRYPOINT ["timeout", "15", "node"]
