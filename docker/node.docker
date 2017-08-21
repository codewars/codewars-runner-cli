# RUN-USING:      docker run --rm --name=node codewars/node-runner --help
# EXAMPLE USAGE:  docker run --rm codewars/node-runner run -l javascript -c "console.log(1+1)"

# Pull base image.
FROM codewars/base-runner

# Install additional libraries
# NOTE: due to this issue https://github.com/npm/npm/issues/9863 we need to install everything at once, which sucks
# because we lose the ability to quickly add new ones as a new layer
# NOTE: we dont configure these via package.json in order to prevent having to re-install them on each code change

# Install Node manager so that we can have multiple versions of node
RUN npm -g install n
RUN n 0.10.33
RUN n 8.1.3
# 6.x must come last since it's the version for the runner itself
RUN n 6.11.0

# Install Coffeescript
RUN npm -g install "coffee-script@^1.12"

# install TypeScript type definitions
RUN npm install -gq "typings@^2"

# Install TypeScript
RUN npm -g install "typescript@^2.4"

RUN set -ex; \
  mkdir -p /runner/frameworks/javascript/angular; \
  cd /runner/frameworks/javascript/angular; \
  wget -xnH -nv \
    https://code.angularjs.org/1.2.9/angular-animate.js \
    https://code.angularjs.org/1.2.9/angular-cookies.js \
    https://code.angularjs.org/1.2.9/angular-mocks.js \
    https://code.angularjs.org/1.2.9/angular-resource.js \
    https://code.angularjs.org/1.2.9/angular-route.js \
    https://code.angularjs.org/1.2.9/angular-sanitize.js \
    https://code.angularjs.org/1.2.9/angular-touch.js \
    https://code.angularjs.org/1.2.9/angular.js \
    \
    https://code.angularjs.org/1.3.9/angular-animate.js \
    https://code.angularjs.org/1.3.9/angular-aria.js \
    https://code.angularjs.org/1.3.9/angular-cookies.js \
    https://code.angularjs.org/1.3.9/angular-messages.js \
    https://code.angularjs.org/1.3.9/angular-mocks.js \
    https://code.angularjs.org/1.3.9/angular-resource.js \
    https://code.angularjs.org/1.3.9/angular-route.js \
    https://code.angularjs.org/1.3.9/angular-sanitize.js \
    https://code.angularjs.org/1.3.9/angular-touch.js \
    https://code.angularjs.org/1.3.9/angular.js \
    \
    https://code.angularjs.org/1.4.9/angular-animate.js \
    https://code.angularjs.org/1.4.9/angular-aria.js \
    https://code.angularjs.org/1.4.9/angular-cookies.js \
    https://code.angularjs.org/1.4.9/angular-message-format.js \
    https://code.angularjs.org/1.4.9/angular-messages.js \
    https://code.angularjs.org/1.4.9/angular-mocks.js \
    https://code.angularjs.org/1.4.9/angular-resource.js \
    https://code.angularjs.org/1.4.9/angular-route.js \
    https://code.angularjs.org/1.4.9/angular-sanitize.js \
    https://code.angularjs.org/1.4.9/angular-touch.js \
    https://code.angularjs.org/1.4.9/angular.js \
    \
    https://code.angularjs.org/1.5.11/angular-animate.js \
    https://code.angularjs.org/1.5.11/angular-aria.js \
    https://code.angularjs.org/1.5.11/angular-cookies.js \
    https://code.angularjs.org/1.5.11/angular-loader.js \
    https://code.angularjs.org/1.5.11/angular-message-format.js \
    https://code.angularjs.org/1.5.11/angular-messages.js \
    https://code.angularjs.org/1.5.11/angular-mocks.js \
    https://code.angularjs.org/1.5.11/angular-parse-ext.js \
    https://code.angularjs.org/1.5.11/angular-resource.js \
    https://code.angularjs.org/1.5.11/angular-route.js \
    https://code.angularjs.org/1.5.11/angular-sanitize.js \
    https://code.angularjs.org/1.5.11/angular-scenario.js \
    https://code.angularjs.org/1.5.11/angular-touch.js \
    https://code.angularjs.org/1.5.11/angular.js \
    \
    https://code.angularjs.org/1.5.8/angular-animate.js \
    https://code.angularjs.org/1.5.8/angular-aria.js \
    https://code.angularjs.org/1.5.8/angular-cookies.js \
    https://code.angularjs.org/1.5.8/angular-message-format.js \
    https://code.angularjs.org/1.5.8/angular-messages.js \
    https://code.angularjs.org/1.5.8/angular-mocks.js \
    https://code.angularjs.org/1.5.8/angular-parse-ext.js \
    https://code.angularjs.org/1.5.8/angular-resource.js \
    https://code.angularjs.org/1.5.8/angular-route.js \
    https://code.angularjs.org/1.5.8/angular-sanitize.js \
    https://code.angularjs.org/1.5.8/angular-touch.js \
    https://code.angularjs.org/1.5.8/angular.js \
    \
    https://code.angularjs.org/1.6.5/angular-animate.js \
    https://code.angularjs.org/1.6.5/angular-aria.js \
    https://code.angularjs.org/1.6.5/angular-cookies.js \
    https://code.angularjs.org/1.6.5/angular-loader.js \
    https://code.angularjs.org/1.6.5/angular-message-format.js \
    https://code.angularjs.org/1.6.5/angular-messages.js \
    https://code.angularjs.org/1.6.5/angular-mocks.js \
    https://code.angularjs.org/1.6.5/angular-parse-ext.js \
    https://code.angularjs.org/1.6.5/angular-resource.js \
    https://code.angularjs.org/1.6.5/angular-route.js \
    https://code.angularjs.org/1.6.5/angular-sanitize.js \
    https://code.angularjs.org/1.6.5/angular-scenario.js \
    https://code.angularjs.org/1.6.5/angular-touch.js \
    https://code.angularjs.org/1.6.5/angular.js; \
  chmod -R a+r .;


RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install && npm install --only=dev && npm dedupe

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/javascript/ lib/runners/javascript/
COPY lib/runners/coffeescript.js lib/runners/
COPY lib/runners/typescript.js lib/runners/
COPY examples/javascript.yml examples/
COPY examples/coffeescript.yml examples/
COPY examples/typescript.yml examples/
COPY test/runner.js test/
COPY test/runners/javascript/ test/runners/javascript/
COPY test/runners/coffeescript_spec.js test/runners/
COPY test/runners/typescript_spec.js test/runners/
COPY frameworks/javascript/*.js frameworks/javascript/

# add display as if it is a node module, so that it can easily be included via require("display")
COPY frameworks/javascript/display.js /runner/node_modules/display/index.js
COPY frameworks/javascript/chai-display.js /runner/node_modules/chai-display/index.js
COPY frameworks/javascript/display.js /runner/node_modules/chai-display/display.js

# Add entire typings/ for now for backward compatibility
COPY typings /runner/typings
# add typings into a cleaner path for inclusion
COPY typings/main/ambient /runner/typings

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
ENV NODE_PATH=/usr/lib/node_modules:/runner/node_modules

# Run the test suite to make sure this thing works
RUN mocha -t 10000 --recursive test/runners/javascript/
RUN mocha -t 10000 test/runners/coffeescript_spec.js
RUN mocha -t 10000 test/runners/typescript_spec.js

ENTRYPOINT ["node"]
