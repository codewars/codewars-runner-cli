# Pull base image.
FROM codewars/base-runner

# Install "bf" Fast Brainf**k Interpreter
RUN apt-get update && apt-get install -y --no-install-recommends bf

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/bf.js lib/runners/
COPY examples/bf.yml examples/
COPY frameworks/bf frameworks/bf
COPY frameworks/javascript/cw-2.js frameworks/javascript/
COPY frameworks/javascript/display.js frameworks/javascript/
COPY frameworks/javascript/chai-display.js frameworks/javascript/
COPY test/runner.js test/
COPY test/runners/bf_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior

RUN mocha -t 10000 test/runners/bf_spec.js

ENTRYPOINT ["node"]
