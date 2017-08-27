FROM codewars/base-runner

# Install Oracle JDK 8
RUN apt-get update \
 && apt-get install -y --no-install-recommends software-properties-common \
 && add-apt-repository ppa:webupd8team/java \
 && apt-get update \
# http://askubuntu.com/a/190674
 && echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections \
 && apt-get install -y --no-install-recommends oracle-java8-installer

# Install Gradle 4.1
RUN set -o errexit -o nounset -o xtrace \
 && apt-get update \
 && apt-get install -y --no-install-recommends unzip \
 && cd /tmp \
 && wget -q https://services.gradle.org/distributions/gradle-4.1-bin.zip -O gradle.zip \
 && echo "d55dfa9cfb5a3da86a1c9e75bb0b9507f9a8c8c100793ccec7beb6e259f9ed43 *gradle.zip" | sha256sum --check - \
 && mkdir -p /opt/gradle \
 && unzip -d /opt/gradle gradle.zip \
 && rm gradle.zip
ENV PATH=/opt/gradle/gradle-4.1/bin:$PATH

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY frameworks/gradle frameworks/gradle
RUN chown -R codewarrior:codewarrior frameworks/gradle

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior

# JVM flags to control memory usage. org.gradle.jvmargs applies to forked JVM process
ENV GRADLE_OPTS="-Xmx64m -Dorg.gradle.jvmargs='-Xmx256m'"

RUN cd /runner/frameworks/gradle \
# download dependencies
 && gradle test || true \
# show dependency tree
 && gradle dependencies

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY test/runner.js test/
COPY frameworks/gradle/prewarm.sh prewarm.sh

COPY lib/runners/kotlin.js lib/runners/
COPY examples/kotlin.yml examples/
COPY test/runners/kotlin_spec.js test/runners/

COPY lib/runners/groovy.js lib/runners/
COPY examples/groovy.yml examples/
COPY test/runners/groovy_spec.js test/runners/

COPY lib/runners/scala.js lib/runners/
COPY examples/scala.yml examples/
COPY test/runners/scala_spec.js test/runners/

RUN mocha test/runners/kotlin_spec.js
RUN mocha test/runners/groovy_spec.js
RUN mocha test/runners/scala_spec.js

ENTRYPOINT ["node"]
