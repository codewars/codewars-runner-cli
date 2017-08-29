HOSTNAME=codewars

CONTAINERS=node dotnet jvm java python ruby alt rust julia systems dart crystal ocaml swift haskell objc go lua esolangs chapel nim r erlang elixir powershell gradle solidity

ALL_CONTAINERS=${CONTAINERS} base

.PHONY: ${ALL_CONTAINERS} clean docker_rm docker_rmi

all: ${CONTAINERS}

recent: ${RECENT_CONTAINERS}

base:
	cp docker/$@.docker ./Dockerfile
	docker build -t $(HOSTNAME)/$@-runner .

${CONTAINERS}:
	cp docker/$@.docker ./Dockerfile
	docker build -t $(HOSTNAME)/$@-runner .

# Kill all of the in-flight and exited docker containers
docker_rm:
	docker ps -q | xargs docker stop
	[ ! -n "$(shell docker ps -a -q)" ] || echo $(shell docker ps -a -q) | xargs -n 1 docker rm -f

# Kill all docker images
docker_rmi: docker_rm
	docker rmi $(docker images -q -f dangling=true)

clean: docker_rmi

deep-clean: docker_rmi

push:
	docker push codewars/base-runner
	docker push codewars/node-runner
	docker push codewars/ruby-runner
	docker push codewars/python-runner
	docker push codewars/dotnet-runner
	docker push codewars/jvm-runner
	docker push codewars/java-runner
	docker push codewars/haskell-runner
	docker push codewars/systems-runner
	docker push codewars/erlang-runner
	docker push codewars/alt-runner
	docker push codewars/rust-runner
	docker push codewars/crystal-runner
	docker push codewars/dart-runner
	docker push codewars/ocaml-runner
	docker push codewars/objc-runner
	docker push codewars/swift-runner || true

pull:
	docker pull codewars/base-runner
	docker pull codewars/node-runner
	docker pull codewars/ruby-runner
	docker pull codewars/python-runner
	docker pull codewars/dotnet-runner
	docker pull codewars/jvm-runner
	docker pull codewars/java-runner
	docker pull codewars/haskell-runner
	docker pull codewars/systems-runner
	docker pull codewars/erlang-runner
	docker pull codewars/alt-runner
	docker pull codewars/rust-runner
	docker pull codewars/crystal-runner
	docker pull codewars/dart-runner
	docker pull codewars/ocaml-runner
	docker pull codewars/objc-runner
	docker pull codewars/swift-runner || true
