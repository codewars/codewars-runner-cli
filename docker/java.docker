# BUILD-USING:    docker build -t codewars/runner-jvm .
# EXAMPLE USAGE:  (Hello world): docker run --rm --name=runner-jvm codewars/runner-jvm run -l java -c $'class Solution {\npublic static void main(String[] args){\nSystem.out.println("Hello world");\n}}'

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

RUN apt-get install -y unzip

# Install dependencies binaries
RUN mkdir -p /usr/local
WORKDIR /usr/local

# Checkout binaries
RUN  wget -q https://dl.bintray.com/groovy/maven/apache-groovy-binary-2.4.10.zip && unzip apache-groovy-binary-2.4.10.zip

# Setup Groovy
RUN mv groovy-2.4.10 groovy
ENV GROOVY_HOME /usr/local/groovy/
ENV PATH /usr/local/groovy/bin:${PATH}

# Install zip utils
RUN apt-get install -y zip unzip --no-install-recommends

# Install Gradle
ENV GRADLE_HOME /usr/local/gradle
ENV GRADLE_VERSION 4.0

ARG GRADLE_DOWNLOAD_SHA256=56bd2dde29ba2a93903c557da1745cafd72cdd8b6b0b83c05a40ed7896b79dfe
RUN set -o errexit -o nounset \
	&& echo "Downloading Gradle" \
	&& wget --no-verbose --output-document=gradle.zip "https://services.gradle.org/distributions/gradle-${GRADLE_VERSION}-bin.zip" \
	\
	&& echo "Checking download hash" \
	&& echo "${GRADLE_DOWNLOAD_SHA256} *gradle.zip" | sha256sum --check - \
	\
	&& echo "Installing Gradle" \
	&& unzip gradle.zip \
	&& rm gradle.zip \
	&& mv "gradle-${GRADLE_VERSION}" "${GRADLE_HOME}/" \
	&& ln --symbolic "${GRADLE_HOME}/bin/gradle" /usr/bin/gradle

RUN mkdir /usr/local/.gradle \
 && chown codewarrior /usr/local/.gradle


RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/java.js lib/runners/
COPY examples/java.yml examples/
COPY test/runner.js test/
COPY test/runners/java_spec.js test/runners/
COPY frameworks/java/prewarm.sh prewarm.sh

COPY frameworks/java frameworks/java
RUN chown codewarrior frameworks/java
RUN gradle --version

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior

# pre-install the default referenced java libraries
RUN cd /runner/frameworks/java && gradle --stacktrace --no-daemon compileTestJava

RUN mocha -t 20000 test/runners/java_spec.js

ENTRYPOINT ["node"]
