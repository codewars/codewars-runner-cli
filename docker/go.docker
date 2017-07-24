FROM codewars/base-runner

# <https://github.com/docker-library/golang/blob/master/1.8/stretch/Dockerfile>
# gcc for cgo
RUN apt-get update && apt-get install -y --no-install-recommends \
		g++ \
		gcc \
		libc6-dev \
		make \
		pkg-config \
	&& rm -rf /var/lib/apt/lists/*

ENV GOLANG_VERSION 1.8
ENV GOLANG_DOWNLOAD_URL https://golang.org/dl/go$GOLANG_VERSION.linux-amd64.tar.gz
ENV GOLANG_DOWNLOAD_SHA256 53ab94104ee3923e228a2cb2116e5e462ad3ebaeea06ff04463479d7f12d27ca

RUN curl -fsSL "$GOLANG_DOWNLOAD_URL" -o golang.tar.gz \
	&& echo "$GOLANG_DOWNLOAD_SHA256  golang.tar.gz" | sha256sum -c - \
	&& tar -C /usr/local -xzf golang.tar.gz \
	&& rm golang.tar.gz

ENV GOPATH /go
ENV PATH $GOPATH/bin:/usr/local/go/bin:$PATH

RUN mkdir -p "$GOPATH/src" "$GOPATH/bin" && chmod -R 777 "$GOPATH"

RUN go get github.com/onsi/ginkgo/ginkgo  # installs the ginkgo CLI
RUN go get github.com/onsi/gomega         # fetches the matcher library


RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

# ADD cli-runner
WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/go.js lib/runners/
COPY examples/go.yml examples/
COPY test/runner.js test/
COPY test/runners/go_spec.js test/runners/

COPY frameworks/go/codewars $GOPATH/src/codewars
RUN go install codewars/reporter

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
RUN mocha -t 5000 test/runners/go_spec.js

ENTRYPOINT ["node"]
