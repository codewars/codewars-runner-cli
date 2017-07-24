# EXAMPLE USAGE:  docker run --rm codewars/julia-runner run -l julia -c "print 1+1 "


FROM codewars/base-runner
#install julia
RUN apt-get update \
  && apt-get install -y --no-install-recommends ca-certificates git \
  && rm -rf /var/lib/apt/lists/*

# Set environment variables
ENV USER codewarrior
ENV HOME /home/codewarrior
ENV JULIA_PATH /usr/local/julia
ENV JULIA_VERSION 0.6.0

RUN mkdir $JULIA_PATH \
  && apt-get update && apt-get install -y curl \
  && curl -sSL "https://julialang-s3.julialang.org/bin/linux/x64/${JULIA_VERSION%[.-]*}/julia-${JULIA_VERSION}-linux-x86_64.tar.gz" -o julia.tar.gz \
  && curl -sSL "https://julialang-s3.julialang.org/bin/linux/x64/${JULIA_VERSION%[.-]*}/julia-${JULIA_VERSION}-linux-x86_64.tar.gz.asc" -o julia.tar.gz.asc \
  && export GNUPGHOME="$(mktemp -d)" \
# http://julialang.org/juliareleases.asc
# Julia (Binary signing key) <buildbot@julialang.org>
  && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys 3673DF529D9049477F76B37566E3C7DC03D6E495 \
  && gpg --batch --verify julia.tar.gz.asc julia.tar.gz \
  && rm -r "$GNUPGHOME" julia.tar.gz.asc \
  && tar -xzf julia.tar.gz -C $JULIA_PATH --strip-components 1 \
  && rm -rf /var/lib/apt/lists/* julia.tar.gz*

ENV PATH $JULIA_PATH/bin:$PATH

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/julia.js lib/runners/
COPY examples/julia.yml examples/
COPY test/runner.js test/
COPY test/runners/julia_spec.js test/runners/
COPY frameworks/julia frameworks/julia

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
RUN mocha -t 5000 test/runners/julia_spec.js

ENTRYPOINT ["node"]
