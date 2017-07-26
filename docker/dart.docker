FROM codewars/base-runner

# Setup the pre-requisites / install dart TODO: REMOVE GIT
RUN \
  apt-get -q update && apt-get install --no-install-recommends -y -q ca-certificates apt-transport-https && \
  curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - && \
  curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list && \
  apt-get update && \
  apt-get install dart=1.23.0-1 && \
  rm -rf /var/lib/apt/lists/*

# Setup dart envs
ENV DART_SDK /usr/lib/dart
ENV PATH $DART_SDK/bin:$PATH

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
ENV PATH=$PATH:$HOME/.pub-cache/bin

# Do dart package setup and pub get etc
RUN mkdir -p /tmp/dart \
 && chmod 0777 /tmp/dart \
 && cd /tmp/dart \
 && pub global activate den \
 && den spec --force \
 && den install test matcher \
 && pub get

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/dart.js lib/runners/
COPY examples/dart.yml examples/
COPY test/runner.js test/
COPY test/runners/dart_spec.js test/runners/

RUN mocha -t 10000 test/runners/dart_spec.js

ENTRYPOINT ["node"]
