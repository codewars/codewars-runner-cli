FROM codewars/base-runner

# Set the locale
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN cd /tmp \
 && wget -q https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb \
 && dpkg -i erlang-solutions_1.0_all.deb \
 && rm erlang-solutions_1.0_all.deb \
 && apt-get update -q \
# && apt-cache showpkg esl-erlang \
# && apt-cache showpkg elixir \
 && apt-get install -q -y --no-install-recommends 'esl-erlang=1:18.3' 'elixir=1.2.4-*'

# Install local Elixir hex and rebar
RUN mix local.hex --force \
 && mix local.rebar --force

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/elixir.js lib/runners/
COPY examples/elixir.yml examples/
COPY frameworks/elixir frameworks/elixir
COPY test/runner.js test/
COPY test/runners/elixir_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior

RUN mocha -t 5000 test/runners/elixir_spec.js

ENTRYPOINT ["node"]
