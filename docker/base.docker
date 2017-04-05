# BUILD-USING:    docker build -t codewars/runner-base .
# TEST-USING:     docker run --rm -i -t --name=test-runner-base --entrypoint=/bin/bash codewars/runner-base -s
# RUN-USING:      docker run --rm --name=runner-base codewars/runner-base --help

# Pull base image.
FROM buildpack-deps:trusty

RUN apt-get update -qq

# gpg keys listed at https://github.com/nodejs/node
RUN set -ex \
  && for key in \
    9554F04D7259F04124DE6B476D5A82AC7E37093B \
    94AE36675C464D64BAFA68DD7434390BDBE9B9C5 \
    0034A06D9D9B0064CE8ADF6BF1747F4AD2306D93 \
    FD3A5288F042B6850C66B31F09FE44734EB7990E \
    71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 \
    DD8F2338BAE7501E3DD5AC78C273792F7D83545D \
    B9AE9905FFD7803F25714661B63B535A4C206CA9 \
    C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 \
  ; do \
    gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key"; \
  done

ENV NPM_CONFIG_LOGLEVEL info
ENV NODE_VERSION 6.6.0

# Install NodeJS
RUN curl -SLO "https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-x64.tar.xz" \
  && curl -SLO "https://nodejs.org/dist/v$NODE_VERSION/SHASUMS256.txt.asc" \
  && gpg --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc \
  && grep " node-v$NODE_VERSION-linux-x64.tar.xz\$" SHASUMS256.txt | sha256sum -c - \
  && tar -xJf "node-v$NODE_VERSION-linux-x64.tar.xz" -C /usr/local --strip-components=1 \
  && rm "node-v$NODE_VERSION-linux-x64.tar.xz" SHASUMS256.txt.asc SHASUMS256.txt

# Set the env variables to non-interactive
ENV DEBIAN_FRONTEND noninteractive
ENV DEBIAN_PRIORITY critical
ENV DEBCONF_NOWARNINGS yes
ENV TERM linux
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections

# ADD codewarrior user
RUN useradd codewarrior
RUN rm -rf ~codewarrior && cp -a ~root ~codewarrior && chown -R codewarrior:codewarrior ~codewarrior

# Define default command.
CMD ["bash"]

# Append any relevant run args as per the help

ENV NODE_ENV production
ENTRYPOINT ["timeout", "15", "node"]

# Install Prereqs
RUN apt-get install -y python-minimal git software-properties-common

# Install Redis
RUN apt-get install -y redis-server

# Install SQLITE
RUN apt-get install -y sqlite libsqlite3-dev

# Install MongoDB
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
RUN echo "deb http://repo.mongodb.org/apt/ubuntu trusty/mongodb-org/3.0 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.0.list
RUN apt-get update && apt-get install -y mongodb-org

# Install MariaDB
RUN apt-key adv --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 0xcbcb082a1bb943db
RUN add-apt-repository 'deb http://mirror.jmu.edu/pub/mariadb/repo/5.5/ubuntu trusty main'
RUN apt-get update && apt-get install -y mariadb-server
RUN service mysql stop
RUN mysql_install_db

# Install Postgres
RUN sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list'
RUN wget -q https://www.postgresql.org/media/keys/ACCC4CF8.asc -O - | sudo apt-key add -
RUN sudo apt-get update && apt-get install -y postgresql postgresql-contrib
ENV PATH /usr/lib/postgresql/9.6/bin:$PATH
RUN mkdir /pgdata
RUN chown codewarrior /pgdata
USER codewarrior
RUN /usr/lib/postgresql/9.6/bin/initdb -D /pgdata/pg
ENV PGDATA /pgdata/pg
USER root
RUN usermod -G postgres codewarrior

# Install RabbitMQ
# RUN echo 'deb http://www.rabbitmq.com/debian/ testing main' | sudo tee /etc/apt/sources.list.d/rabbitmq.list
# RUN wget -O- https://www.rabbitmq.com/rabbitmq-release-signing-key.asc | sudo apt-key add -
# RUN apt-get update && apt-get install -y rabbitmq-server

# Install CouchDB
# RUN add-apt-repository ppa:couchdb/stable -y
# RUN apt-get update && apt-get install -y couchdb

# Install Cairo graphics
# RUN apt-get install -y libcairo2-dev libjpeg8-dev libpango1.0-dev libgif-dev build-essential g++

RUN mkdir /.npm-global
RUN npm config set prefix '/.npm-global'
RUN su codewarrior -c "echo 'export PATH=\"/.npm-global/bin:$PATH\"' >> ~/.profile"
ENV PATH /.npm-global/bin:$PATH

# Install Node testing frameworks & additional frameworks
RUN npm install -g chai mocha

