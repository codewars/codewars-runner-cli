# BUILD-USING:    docker build -t codewars/runner-haskell .
# TEST-USING:     docker run --rm -i -t --name=test-runner-haskell --entrypoint=/bin/bash codewars/runner-haskell -s
# RUN-USING:      docker run --rm --name=runner-haskell codewars/runner-haskell --help

# Pull base image.
FROM codewars/base-runner

# =========================================================================
# Haskell stuff begin
# -------------------------------------------------------------------------

RUN add-apt-repository ppa:hvr/ghc
RUN apt-get update

# Install Haskell System
ENV RESOLVER=lts-6.24 GHC_VER=7.10.3 CABAL_VER=1.22 HAPPY_VER=1.19.5

RUN apt-get install -y ghc-${GHC_VER} cabal-install-${CABAL_VER} happy-${HAPPY_VER}

USER codewarrior
ENV HOME /home/codewarrior
ENV PATH=${HOME}/.cabal/bin:/opt/ghc/${GHC_VER}/bin:/opt/cabal/${CABAL_VER}/bin:/opt/happy/${HAPPY_VER}/bin:${PATH}
RUN mkdir ${HOME}/.cabal ${HOME}/.cabal/bin
RUN echo "ghc-options: -fno-warn-tabs" >> ${HOME}/.cabal/config
RUN echo "remote-repo: stackage_${RESOLVER}:http://www.stackage.org/${RESOLVER}" >> ${HOME}/.cabal/config
RUN echo "remote-repo-cache: /home/codewarrior/.cabal/packages" >> ${HOME}/.cabal/config
RUN cabal update
RUN cabal install cabal-install

# Install Haskell Packages
# To install additional packages use "RUN cabal install <pkg-name>"
RUN cabal install split ifelse
RUN cabal install persistent-sqlite persistent-template
RUN cabal install haskell-src-exts lens
RUN cabal install hspec hspec-core hspec-discover

# -------------------------------------------------------------------------
# Haskell stuff end.
# =========================================================================

USER root
RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/haskell.js lib/runners/
COPY examples/haskell.yml examples/
COPY test/runner.js test/
COPY test/runners/haskell_spec.js test/runners/
COPY frameworks/haskell frameworks/haskell

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
RUN mocha -t 10000 test/runners/haskell_spec.js

ENTRYPOINT ["node"]
