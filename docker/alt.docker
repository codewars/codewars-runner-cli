# Pull base image.
FROM codewars/base-runner

# Install PHP
# LC_ALL=C.UTF-8 workarounds the unicode issue for `add-apt-repository`
# see <https://github.com/oerdnj/deb.sury.org/issues/56>
RUN apt-get update && apt-get install -y software-properties-common \
    && LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php \
    && apt-get update && apt-get install -y php7.0 php7.0-xml

RUN curl -fsSL https://phar.phpunit.de/phpunit-5.7.phar -o phpunit.phar \
	&& chmod 755 phpunit.phar \
	&& mv phpunit.phar /usr/local/bin/ \
	&& ln -s /usr/local/bin/phpunit.phar /usr/local/bin/phpunit

# Install Prolog
#RUN apt-add-repository ppa:swi-prolog/stable && apt-get update
#RUN apt-get install -y swi-prolog

# Install GNU R
RUN apt-get install -y r-base

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/php.js lib/runners/
COPY lib/runners/r.js lib/runners/
COPY examples/php.yml examples/
COPY frameworks/php frameworks/php
COPY test/runner.js test/
COPY test/runners/php_spec.js test/runners/
COPY test/runners/r_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
RUN mocha -t 3000 test/runners/php_spec.js

ENTRYPOINT ["node"]
