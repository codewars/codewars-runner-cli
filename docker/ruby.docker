# BUILD-USING:    docker build -t codewars/runner-ruby .
# TEST-USING:     docker run --rm -i -t --name=test-runner-ruby --entrypoint=/bin/bash codewars/runner-ruby -s
# RUN-USING:      docker run --rm --name=runner-ruby codewars/runner-ruby --help
# EXAMPLE USAGE:  docker run --rm codewars/runner-ruby run -l ruby -c "puts 1+1"

# Pull base image.
FROM codewars/base-runner

#Install ruby
# skip installing gem documentation
RUN mkdir -p /usr/local/etc \
	&& { \
		echo 'install: --no-document'; \
		echo 'update: --no-document'; \
	} >> /usr/local/etc/gemrc

ENV RUBY_MAJOR 2.3
ENV RUBY_VERSION 2.3.0
ENV RUBY_DOWNLOAD_SHA256 ba5ba60e5f1aa21b4ef8e9bf35b9ddb57286cb546aac4b5a28c71f459467e507
ENV RUBYGEMS_VERSION 2.6.1

# some of ruby's build scripts are written in ruby
# we purge this later to make sure our final image uses what we just built
RUN set -ex \
	&& buildDeps=' \
		bison \
		libgdbm-dev \
		ruby \
	' \
	&& apt-get update \
	&& apt-get install -y --no-install-recommends $buildDeps \
	&& rm -rf /var/lib/apt/lists/* \
	&& curl -fSL -o ruby.tar.gz "http://cache.ruby-lang.org/pub/ruby/$RUBY_MAJOR/ruby-$RUBY_VERSION.tar.gz" \
	&& echo "$RUBY_DOWNLOAD_SHA256 *ruby.tar.gz" | sha256sum -c - \
	&& mkdir -p /usr/src/ruby \
	&& tar -xzf ruby.tar.gz -C /usr/src/ruby --strip-components=1 \
	&& rm ruby.tar.gz \
	&& cd /usr/src/ruby \
	&& { echo '#define ENABLE_PATH_CHECK 0'; echo; cat file.c; } > file.c.new && mv file.c.new file.c \
	&& autoconf \
	&& ./configure --disable-install-doc \
	&& make -j"$(nproc)" \
	&& make install \
	&& apt-get purge -y --auto-remove $buildDeps \
	&& gem update --system $RUBYGEMS_VERSION \
	&& rm -r /usr/src/ruby

ENV BUNDLER_VERSION 1.11.2

RUN gem install bundler --version "$BUNDLER_VERSION"

# install things globally, for great justice
# and don't create ".bundle" in all our apps
ENV GEM_HOME /usr/local/bundle
ENV BUNDLE_PATH="$GEM_HOME" \
	BUNDLE_BIN="$GEM_HOME/bin" \
	BUNDLE_SILENCE_ROOT_WARNING=1 \
	BUNDLE_APP_CONFIG="$GEM_HOME"
ENV PATH $BUNDLE_BIN:$PATH
RUN mkdir -p "$GEM_HOME" "$BUNDLE_BIN" \
	&& chmod 777 "$GEM_HOME" "$BUNDLE_BIN"

# needed for nokogiri
RUN apt-get -y install zlib1g-dev

# RSpec Gems for Testing
RUN gem install --no-ri --no-rdoc \
	rspec \
	rspec-its \
	rspec-rails \
	\
	amqp \
	bunny \
	capybara \
	celluloid \
	concerning \
	concurrent-ruby \
	connection_pool \
	couchrest \
	couch_potato \
	elasticsearch \
	eventmachine \
	faraday \
	jbuilder \
	jwt \
	hashie \
	httparty \
	factory_girl \
	faker \
	ffi-rzmq \
	googlecharts \
	pg \
	pry \
	mock_redis \
	mongo \
	mongoid \
	net-ssh \
	nokogiri \
	rails \
	redis \
	ruby-graphviz \
	sequel \
	sinatra \
	timecop \
	timers \
	webmock

# reduced selection of sciruby-full gems since all needed build dependencies are not installed
RUN gem install --no-ri --no-rdoc \
	sciruby \
	ai4r \
	algorithms \
	awesome_print \
	classifier \
	daru \
	darwinning \
	decisiontree \
	distribution \
	gga4r \
	gimuby \
	hamster \
	histogram \
	measurable \
	mikon \
	minimization \
	narray \
	ruby-fann \
	statsample \
	statsample-glm \
	statsample-timeseries \
	stuff-classifier \
	symbolic \
	unit \
	chronic \
	ably \
	bcrypt

USER codewarrior

# Sample Database
# http://www.postgresqltutorial.com/postgresql-sample-database/#

ADD sample_data /runner/sample_data
RUN /usr/lib/postgresql/9.6/bin/pg_ctl -w start \
    && createdb -U codewarrior spec \
    && createdb -U codewarrior dvdrental \
    && pg_restore -U codewarrior -d dvdrental -v /runner/sample_data/dvdrental.tar || true \
#    && psql -f /runner/sample_data/mlb-samples.sql sports codewarrior \
    && psql -l \
    && /usr/lib/postgresql/9.6/bin/pg_ctl -w stop

USER root

# install the tcsh shell
RUN apt-get update && apt-get -y install csh tcsh

# Install extra shell packages
RUN apt-get update && apt-get -y install mlocate bc

# allow codewarrior to install gems
RUN chown codewarrior /usr/local/bundle


RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/ruby/ lib/runners/ruby/
COPY lib/runners/shell.js lib/runners/
COPY lib/runners/sql.js lib/runners/
COPY examples/ruby.yml examples/
COPY examples/sql.yml examples/
COPY frameworks/ruby frameworks/ruby
COPY test/runner.js test/
COPY test/runners/ruby_spec.js test/runners/
COPY test/runners/shell_spec.js test/runners/
COPY test/runners/sql_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior

RUN mocha -t 5000 test/runners/ruby_spec.js
RUN mocha -t 5000 test/runners/shell_spec.js
#RUN mocha -t 5000 test/runners/sql_spec.js
RUN sh /runner/lib/cleanup.sh

ENTRYPOINT ["node"]
