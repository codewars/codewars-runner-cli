FROM codewars/base-runner

# Install OCAML 4.02.03 + OPAM 1.2.2
RUN apt-get install -y software-properties-common \
 && add-apt-repository -y ppa:avsm/ocaml42+opam12 \
 && apt-get -y update \
 && apt-get -y install camlp4 ocaml opam

RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/ocaml.js lib/runners/
COPY examples/ocaml.yml examples/
COPY test/runner.js test/
COPY test/runners/ocaml_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior

WORKDIR /workspace
ENV OPAMEXTERNALSOLVER=/usr/bin/aspcud
RUN opam init -y \
 && eval `opam config env` \
 && opam update -y \
 && opam install -y \
      batteries.2.5.3 \
      ounit.2.0.0 \
      core.113.33.03 \
      lwt.3.1.0 \
      utop.2.0.1

# opam config env
ENV PATH=$HOME/.opam/system/bin:$PATH \
    OCAML_TOPLEVEL_PATH=$HOME/.opam/system/lib/toplevel \
    PERL5LIB=$HOME/.opam/system/lib/perl5:$PERL5LIB \
    # MANPATH=$MANPATH:$HOME/.opam/system/man
    CAML_LD_LIBRARY_PATH=$HOME/.opam/system/lib/stublibs:/usr/lib/ocaml/stublibs
RUN (echo 'let () ='; \
     echo '  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")'; \
     echo '  with Not_found -> ()'; \
     echo ';;') >> $HOME/.ocamlinit

WORKDIR /runner
RUN mocha -t 5000 test/runners/ocaml_spec.js

ENTRYPOINT ["node"]
