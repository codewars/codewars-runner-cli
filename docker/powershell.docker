# BUILD-USING: docker build -t codewars/runner-powershell .
# TEST-USING: docker run --rm -i -t --name test-runner-powershell --entrypoint=/bin/bash/codewars/runner-powershell -s
# RUN-USING: docker run --rm --name=runner-powershell codewars/runner-powershell --help

# Pull base image.
FROM codewars/base-runner

# Install PowerShell 6.0
RUN set -ex \
 && apt-get -q update \
 && apt-get -q install -y --no-install-recommends libicu52 libunwind8 \
 && cd /tmp \
 && curl -fsSL https://github.com/PowerShell/PowerShell/releases/download/v6.0.0-beta.4/powershell_6.0.0-beta.4-1ubuntu1.14.04.1_amd64.deb -o powershell.deb \
 && dpkg -i powershell.deb \
 && rm powershell.deb \
 && apt-get install -f \
 && rm -rf /var/lib/apt/lists/*

# Install Pester testing framework
RUN powershell -Command {Install-Module Pester -force}

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/powershell.js lib/runners/
COPY examples/powershell.yml examples/
COPY frameworks/powershell frameworks/powershell
COPY test/runner.js test/
COPY test/runners/powershell_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
RUN mocha -t 5000 test/runners/powershell_spec.js

ENTRYPOINT ["node"]
