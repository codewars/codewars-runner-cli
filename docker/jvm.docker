# BUILD-USING:    docker build -t codewars/runner-jvm .
# TEST-USING:     docker run --rm -i -t --name=test-runner-jvm --entrypoint=/bin/bash codewars/runner-jvm -s
# RUN-USING:      docker run --rm --name=runner-jvm codewars/runner-jvm --help

# Pull base image.
FROM codewars/base-runner

# Needed to run add-apt-repository
RUN apt-get -y install software-properties-common

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
RUN mkdir ~/.lein

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

# Build the jvm-runner
COPY jvm-runner jvm-runner
RUN cd /runner/jvm-runner \
 && LEIN_ROOT=true lein do clean, test, uberjar

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/clojure.js lib/runners/
COPY examples/clojure.yml examples/
COPY test/runner.js test/
COPY test/runners/clojure_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
ENV TIMEOUT 10000
RUN mocha -t 10000 test/runners/clojure_spec.js

ENTRYPOINT ["node"]
