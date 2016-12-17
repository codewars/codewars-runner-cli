# EXAMPLE USAGE:  (FIRST BUILT): docker build -t objc .
# EXAMPLE USAGE:  docker run objc run -l objc -c "NSLog(@\"Codewars Objective C\");"

# Pull base image.
FROM codewars/base-runner

# Optionally mount '/objc-vol' on ephemeral storage vol for faster
# installing of packages, compiling, etc.
env OBJC_VOL /objc-vol
env OBJC_GEN_DIR $OBJC_VOL/objc-gen
env OBJC_SOURCE_DIR $OBJC_VOL/objc-source

# NEW
RUN rm /etc/apt/sources.list
RUN echo "deb http://archive.ubuntu.com/ubuntu/ xenial main restricted" >> /etc/apt/sources.list
RUN echo "deb-src http://archive.ubuntu.com/ubuntu/ xenial main restricted" >> /etc/apt/sources.list
RUN echo "deb http://archive.ubuntu.com/ubuntu/ xenial-updates main restricted" >> /etc/apt/sources.list
RUN echo "deb-src http://archive.ubuntu.com/ubuntu/ xenial-updates main restricted" >> /etc/apt/sources.list
RUN echo "deb http://archive.ubuntu.com/ubuntu/ xenial universe" >> /etc/apt/sources.list
RUN echo "deb-src http://archive.ubuntu.com/ubuntu/ xenial universe" >> /etc/apt/sources.list
RUN echo "deb http://archive.ubuntu.com/ubuntu/ xenial-updates universe" >> /etc/apt/sources.list
RUN echo "deb-src http://archive.ubuntu.com/ubuntu/ xenial-updates universe" >> /etc/apt/sources.list
RUN echo "deb http://archive.ubuntu.com/ubuntu/ xenial-security main restricted" >> /etc/apt/sources.list
RUN echo "deb-src http://archive.ubuntu.com/ubuntu/ xenial-security main restricted" >> /etc/apt/sources.list
RUN echo "deb http://archive.ubuntu.com/ubuntu/ xenial-security universe" >> /etc/apt/sources.list
RUN echo "deb-src http://archive.ubuntu.com/ubuntu/ xenial-security universe" >> /etc/apt/sources.list

RUN apt-get update
RUN apt-get upgrade -y

RUN apt-get install -y build-essential gobjc gobjc++ gnustep gnustep-devel libgnustep-base-dev

# Add the package json first to a tmp directory and build, copy over so that we dont rebuild every time
ADD package.json /tmp/package.json
RUN cd /tmp && npm install --production
RUN mkdir -p /runner && cp -a /tmp/node_modules /runner

# ADD cli-runner and install node deps
ADD . /runner

WORKDIR /runner

# Run the test suite to make sure this thing works

USER codewarrior

# Set environment variables
ENV USER codewarrior
ENV HOME /home/codewarrior
RUN ls -la
RUN mocha -t 5000 test/runners/objc_spec.js

# timeout is a fallback in case an error with node
# prevents it from exiting properly
ENTRYPOINT ["timeout", "15", "node"]
