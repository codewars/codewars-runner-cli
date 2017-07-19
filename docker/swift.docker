# EXAMPLE USAGE:  docker run --rm codewars/swift-runner run -l swift -c "print(1+1)"
FROM codewars/base-runner

# Based on https://github.com/swiftdocker/docker-swift/blob/master/Dockerfile
RUN apt-get -q update && \
    apt-get -q install -y --no-install-recommends \
    clang-3.6 \
    curl \
    git \
    libc6-dev \
    libedit-dev \
    libicu-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2 \
    pkg-config \
    python2.7 \
    python2.7-dev \
    make \
    && update-alternatives --quiet --install /usr/bin/clang clang /usr/bin/clang-3.6 100 \
    && update-alternatives --quiet --install /usr/bin/clang++ clang++ /usr/bin/clang++-3.6 100 \
    && rm -r /var/lib/apt/lists/*

RUN set -ex \
   && export GNUPGHOME="$(mktemp -d)" \
   && \
     for key in \
       # pub   4096R/412B37AD 2015-11-19 [expires: 2017-11-18]
       #       Key fingerprint = 7463 A81A 4B2E EA1B 551F  FBCF D441 C977 412B 37AD
       # uid                  Swift Automatic Signing Key #1 <swift-infrastructure@swift.org>
       7463A81A4B2EEA1B551FFBCFD441C977412B37AD \
       # pub   4096R/21A56D5F 2015-11-28 [expires: 2017-11-27]
       #       Key fingerprint = 1BE1 E29A 084C B305 F397  D62A 9F59 7F4D 21A5 6D5F
       # uid                  Swift 2.2 Release Signing Key <swift-infrastructure@swift.org>
       1BE1E29A084CB305F397D62A9F597F4D21A56D5F \
       # pub   4096R/91D306C6 2016-05-31 [expires: 2018-05-31]
       #       Key fingerprint = A3BA FD35 56A5 9079 C068  94BD 63BC 1CFE 91D3 06C6
       # uid                  Swift 3.x Release Signing Key <swift-infrastructure@swift.org>
       A3BAFD3556A59079C06894BD63BC1CFE91D306C6 \
     ; do \
       gpg --quiet --keyserver ha.pool.sks-keyservers.net --recv-keys "$key"; \
     done \
   && cd /tmp \
   && curl -fSsL https://swift.org/builds/swift-3.1.1-release/ubuntu1404/swift-3.1.1-RELEASE/swift-3.1.1-RELEASE-ubuntu14.04.tar.gz -o swift.tar.gz \
   && curl -fSsL https://swift.org/builds/swift-3.1.1-release/ubuntu1404/swift-3.1.1-RELEASE/swift-3.1.1-RELEASE-ubuntu14.04.tar.gz.sig -o swift.tar.gz.sig \
   && gpg --batch --verify --quiet swift.tar.gz.sig swift.tar.gz \
   && tar -xzf swift.tar.gz --directory / --strip-components=1 \
   && rm -r "$GNUPGHOME" swift.tar.gz.sig swift.tar.gz \
   && rm -rf /var/lib/apt/lists/* \
   && chmod -R o+r /usr/lib/swift


RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/swift.js lib/runners/
COPY examples/swift.yml examples/
COPY frameworks/swift frameworks/swift
COPY test/runner.js test/
COPY test/runners/swift_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
RUN mocha -t 5000 test/runners/swift_spec.js

ENTRYPOINT ["node"]
