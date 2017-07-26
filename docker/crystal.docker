FROM codewars/base-runner

RUN apt-get update -qq
RUN apt-get install apt-transport-https
RUN set -ex \
  && cd /tmp \
  && wget -q -O crystal.tar.gz https://github.com/crystal-lang/crystal/releases/download/0.21.1/crystal-0.21.1-1-linux-x86_64.tar.gz \
  && mkdir -p /opt/crystal \
  && tar zxf crystal.tar.gz -C /opt/crystal --strip-components=1 \
  && rm crystal.tar.gz \
  && ln -s /opt/crystal/embedded/bin/shards /usr/local/bin/shards \
  && ln -s /opt/crystal/bin/crystal /usr/local/bin/crystal


RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn


WORKDIR /runner
COPY package.json package.json
RUN npm install --production

# Install Shards local to the crystal directory that we will be using
COPY frameworks/crystal/shard.yml /home/codewarrior/crystal/shard.yml
COPY frameworks/crystal/formatter.cr /home/codewarrior/crystal/formatter.cr
RUN chown -R codewarrior:codewarrior /home/codewarrior/crystal

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
RUN cd /home/codewarrior/crystal \
 && shards install

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/crystal.js lib/runners/
COPY examples/crystal.yml examples/
COPY test/runner.js test/
COPY test/runners/crystal_spec.js test/runners/

RUN mocha -t 10000 test/runners/crystal_spec.js

ENTRYPOINT ["node"]
