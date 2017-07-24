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
 && apt-get install -q -y --no-install-recommends 'esl-erlang=1:20.0'

RUN cd /usr/local/bin \
 && wget -q https://github.com/erlang/rebar3/releases/download/3.4.2/rebar3 \
 && chmod a+x rebar3


RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production


COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/erlang.js lib/runners/
COPY test/runner.js test/
COPY test/runners/erlang_spec.js test/runners/
COPY examples/erlang.yml examples/
COPY frameworks/erlang/cw /workspace/erlang
RUN chown -R codewarrior:codewarrior /workspace/erlang

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior

# fetch packages if any, compile 'eunit_codewars', run some example tests
# 'eunit_codewars' is compiled again with submitted code since it's in '_checkouts'
ENV REBAR_COLOR=none
RUN cd /workspace/erlang \
 && rebar3 compile \
 # show dependencies
 && rebar3 deps \
 # show project structure
 && rebar3 tree \
 && rebar3 eunit || true \
 && rm -rf src/*.erl test/*.erl _build/test

RUN mocha -t 5000 test/runners/erlang_spec.js

ENTRYPOINT ["node"]
