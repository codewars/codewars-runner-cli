FROM codewars/base-runner

# https://cran.r-project.org/bin/linux/ubuntu/README
RUN set -ex \
 && gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9 \
 && gpg -a --export E084DAB9 | apt-key add - \
 && echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list \
 && apt-get -q update \
 && apt-get -q -y --no-install-recommends install 'r-base=3.4.1-*'

# TODO version lock. need `devtools` package?
RUN R -q -e 'install.packages("testthat", repo="https://cran.rstudio.com/")' && rm -rf /tmp/*

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/r.js lib/runners/
COPY examples/r.yml examples/
COPY frameworks/r frameworks/r
COPY test/runner.js test/
COPY test/runners/r_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
RUN mocha -t 5000 test/runners/r_spec.js

ENTRYPOINT ["node"]
