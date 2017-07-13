# BUILD-USING:    docker build -t codewars/runner-func .
# TEST-USING:     docker run --rm -i -t --name=test-runner-func --entrypoint=/bin/bash codewars/runner-haskell -s
# RUN-USING:      docker run --rm --name=runner-func codewars/runner-func --help

# Pull base image.
FROM codewars/base-runner

# Install Racket
RUN apt-get -y install racket

# Install SBCL (Steel Bank Common Lisp)
RUN apt-get -y install sbcl

# Install OCAML
RUN apt-get -y install ocaml-nox

# add the package json first to a tmp directory and build, copy over so that we dont rebuild every time
ADD package.json /tmp/package.json
RUN cd /tmp && npm install --production
RUN mkdir -p /runner && cp -a /tmp/node_modules /runner

# ADD cli-runner
ADD . /runner
WORKDIR /runner
RUN ln -s /home/codewarrior /workspace

# Run the test suite to make sure this thing works

USER codewarrior
# Set environment variables
ENV USER codewarrior
ENV HOME /home/codewarrior
RUN mocha -t 5000 test/runners/{ocaml,racket}_spec.js

#timeout is a fallback in case an error with node
#prevents it from exiting properly
ENTRYPOINT ["node"]
