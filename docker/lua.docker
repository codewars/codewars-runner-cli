FROM codewars/base-runner

# luarocks with lua 5.2 needs to be built from source
RUN apt-get update && apt-get install -y lua5.2 liblua5.2-dev unzip
RUN curl -fsSL https://github.com/luarocks/luarocks/archive/v2.4.2.tar.gz | tar xz -C /tmp \
	&& cd /tmp/luarocks-2.4.2 \
	&& ./configure --lua-version=5.2 \
	&& make build \
	&& make install \
	&& cd /tmp \
	&& rm -rf /tmp/luarocks-2.4.2

RUN luarocks install busted

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/lua.js lib/runners/
COPY examples/lua.yml examples/
COPY frameworks/lua frameworks/lua
COPY test/runner.js test/
COPY test/runners/lua_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
RUN mocha -t 5000 test/runners/lua_spec.js

ENTRYPOINT ["node"]
