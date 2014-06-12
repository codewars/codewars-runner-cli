# BUILD-USING:    docker build -t codewars/cli-runner .
# TEST-USING:     docker run --rm -i -t --name=test-cli-runner --entrypoint=/bin/bash codewars/cli-runner -s
# RUN-USING:      docker run --rm --name=cli-runner codewars/cli-runner --help
# EXAMPLE USAGE:  docker run --rm codewars/cli-runner run -l ruby -c "puts 1+1"

# Append any relevant run args as per the help

FROM dockerfile/nodejs

RUN apt-get install -y ruby
#
## install bundler
RUN gem install rspec --no-ri --no-rdoc
RUN gem install minitest --no-ri --no-rdoc

# Install Coffeescript
RUN npm -g install coffee-script
RUN npm -g install chai
RUN npm -g install mocha

# Install Python 3


# ADD cli-runner and install node deps
ADD . /cli-runner
WORKDIR /cli-runner
RUN npm install

ENTRYPOINT ["node"]