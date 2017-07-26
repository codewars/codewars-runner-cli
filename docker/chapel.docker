FROM codewars/base-runner

# Set environment variables
ENV CHPL_VERSION 1.15.0
ENV CHPL_HOME /usr/local/chapel-${CHPL_VERSION}

# Download and build Chapel
RUN mkdir $CHPL_HOME \
  && apt-get update && apt-get install -y curl \
  && curl -sSL "https://github.com/chapel-lang/chapel/releases/download/${CHPL_VERSION}/chapel-${CHPL_VERSION}.tar.gz" -o chapel.tar.gz \
  && tar -xzf chapel.tar.gz -C /usr/local \
  && rm -f chapel.tar.gz \
  && cd $CHPL_HOME \
  && make \
  && cd .. \
  && $CHPL_HOME/util/printchplenv
  
ENV CHPL_HOST_PLATFORM=linux64
ENV PATH="$CHPL_HOME"/bin/$CHPL_HOST_PLATFORM:"$CHPL_HOME"/util:"$PATH"

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/chapel.js lib/runners/
COPY examples/chapel.yml examples/
COPY frameworks/chapel frameworks/chapel
COPY test/runner.js test/
COPY test/runners/chapel_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
RUN mocha -t 10000 test/runners/chapel_spec.js

ENTRYPOINT ["node"]
