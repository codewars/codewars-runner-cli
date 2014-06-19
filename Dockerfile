# BUILD-USING:    docker build -t codewars/cli-runner .
# TEST-USING:     docker run --rm -i -t --name=test-cli-runner --entrypoint=/bin/bash codewars/cli-runner -s
# RUN-USING:      docker run --rm --name=cli-runner codewars/cli-runner --help
# EXAMPLE USAGE:  docker run --rm codewars/cli-runner run -l ruby -c "puts 1+1"

# Append any relevant run args as per the help

FROM dockerfile/nodejs
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
#RUN npm -g install mocha

# Install Python 3

# Install Java
RUN apt-get install -y openjdk-6-jdk
#### Install runtime
##RUN apt-get install -y openjdk-6-jre-headless
#### Install aptitude to download package
##RUN apt-get install -y aptitude
####
##RUN aptitude download openjdk-6-jdk; dpkg -i --ignore-depends=openjdk-6-jre openjdk-6-jdk*.deb

# ADD cli-runner and install node deps
ADD . /cli-runner
WORKDIR /cli-runner
RUN npm install

#timeout is a fallback in case an error with node
#prevents it from exiting properly
ENTRYPOINT ["timeout", "15", "node"]
