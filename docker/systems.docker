# BUILD-USING:    docker build -t codewars/runner-systems .
# TEST-USING:     docker run --rm -i -t --name=test-runner-systems --entrypoint=/bin/bash codewars/runner-systems -s
# RUN-USING:      docker run --rm --name=runner-systems codewars/runner-systems --help


# Pull base image.
FROM codewars/base-runner

# Needed to run add-apt-repository
RUN apt-get -y install software-properties-common

# Install CLANG 3.6
RUN add-apt-repository ppa:ubuntu-toolchain-r/ppa
RUN apt-get update
RUN apt-get -y install clang-3.6 lldb-3.6

# Install libc++
RUN apt-get -y install libc++-dev

# Install Criterion Framework
RUN apt-get -y install cmake git
RUN git clone --branch=master https://github.com/Snaipe/Criterion && \
    cd Criterion && \
    git checkout v2.2.2 && \
    mkdir build && cd build && \
    cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo \
          -DCMAKE_INSTALL_PREFIX=/usr \
          -DCMAKE_INSTALL_LIBDIR=lib \
          .. && \
    make && \
    make install && \
    cd ../../ && rm -Rf Criterion

# NASM
# RUN apt-get -y install nasm

# ARM Assembly Emulation
# RUN apt-get -y install gcc-4.7-arm-linux-gnueabi libc6-dev-armel-cross qemu-user

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/c.js lib/runners/
COPY lib/runners/cpp.js lib/runners/
COPY examples/c.yml examples/
COPY examples/cpp.yml examples/
COPY frameworks/c frameworks/c
COPY frameworks/cpp frameworks/cpp
COPY test/runner.js test/
COPY test/runners/c_spec.js test/runners/
COPY test/runners/cpp_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior

RUN mocha -t 5000 test/runners/{c,cpp}_spec.js

ENTRYPOINT ["node"]
