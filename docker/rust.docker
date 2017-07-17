FROM codewars/base-runner

RUN ln -s /home/codewarrior /workspace

COPY frameworks/rust/skeleton /workspace/rust
RUN chown -R codewarrior:codewarrior /workspace/rust

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior

# Install rustup with the Rust v1.15.1 toolchain
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain 1.15.1
# ~/.cargo/env
ENV PATH $HOME/.cargo/bin:$PATH
RUN cd /workspace/rust && cargo build && rm src/lib.rs

USER root
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/rust.js lib/runners/
COPY examples/rust.yml examples/
COPY test/runner.js test/
COPY test/runners/rust_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
ENV PATH=$HOME/.cargo/bin:$PATH

RUN mocha -t 10000 test/runners/rust_spec.js

ENTRYPOINT ["node"]
