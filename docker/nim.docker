FROM codewars/base-runner

RUN apt-get -q update \
 && apt-get -q install -y --no-install-recommends gcc \
 && rm -rf /var/lib/apt/lists/*

ENV NIM_VERSION=0.17.0 CSOURCES_VERSION=0.17.0
RUN set -ex \
 && cd /tmp \
 && curl -fsSL "https://github.com/nim-lang/Nim/archive/v$NIM_VERSION.tar.gz" -o nim.tar.gz \
 && mkdir -p /opt/nim \
 && tar zxf nim.tar.gz -C /opt/nim --strip-components=1 \
 && rm nim.tar.gz \
 && cd /opt/nim \
 && curl -fsSL "https://github.com/nim-lang/csources/archive/v$CSOURCES_VERSION.tar.gz" -o csources.tar.gz \
 && mkdir csources \
 && tar zxf csources.tar.gz -C csources --strip-components=1 \
 && rm csources.tar.gz \
 && cd csources \
 && sh build.sh \
 && cd ../ \
 && ./bin/nim c koch \
 && ./koch boot -d:release \
 && rm -rf ./ci ./csources ./icons ./tests ./web \
 && chmod -R 755 /opt/nim

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/nim.js lib/runners/
COPY examples/nim.yml examples/
COPY frameworks/nim frameworks/nim
COPY test/runner.js test/
COPY test/runners/nim_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior PATH=/opt/nim/bin:$PATH
RUN mocha -t 5000 test/runners/nim_spec.js

ENTRYPOINT ["node"]
