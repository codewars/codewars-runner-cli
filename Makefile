HOSTNAME=codewars

# Building haskell and erlang images have been suspended (frozen) until they are able to be repaired
# CONTAINERS=dotnet jvm node python ruby alt func dart systems erlang haskell
CONTAINERS=dotnet jvm node python ruby alt func dart systems rust julia crystal

ALL_CONTAINERS=${CONTAINERS} base

.PHONY: ${ALL_CONTAINERS} clean docker_rm docker_rmi

all: ${CONTAINERS}

base:
	cp docker/$@.docker ./Dockerfile
	docker build -t $(HOSTNAME)/$@-runner .

${CONTAINERS}: base
	cp docker/$@.docker ./Dockerfile
	docker build -t $(HOSTNAME)/$@-runner .


# Kill all of the in-flight and exited docker containers
docker_rm:
	docker ps -q | xargs docker stop
	[ ! -n "$(shell docker ps -a -q)" ] || echo $(shell docker ps -a -q) | xargs -n 1 docker rm -f

# Kill all docker images
docker_rmi: docker_rm
	[ ! -n "$(shell docker images -q)" ] || docker images -q | xargs -n 1 docker rmi -f

clean: docker_rm_exited docker_rmi_temporary

deep-clean: docker_rmi

pull:
	docker pull codewars/base-runner
	docker pull codewars/ruby-runner
	docker pull codewars/node-runner
	docker pull codewars/python-runner
	docker pull codewars/dotnet-runner
	docker pull codewars/jvm-runner
	docker pull codewars/systems-runner
	docker pull codewars/func-runner
	docker pull codewars/erlang-runner
	docker pull codewars/alt-runner
	docker pull codewars/rust-runner || true
	docker pull codewars/julia-runner || true
	docker pull codewars/crystal-runner || true
	docker pull codewars/dart-runner || true

save:
	docker save codewars/base-runner > ~/docker/image.tar
	docker save codewars/ruby-runner > ~/docker/image.tar
	docker save codewars/node-runner > ~/docker/image.tar
	docker save codewars/python-runner > ~/docker/image.tar
	docker save codewars/dotnet-runner > ~/docker/image.tar
	docker save codewars/jvm-runner > ~/docker/image.tar
	docker save codewars/func-runner > ~/docker/image.tar
	docker save codewars/erlang-runner > ~/docker/image.tar
	docker save codewars/alt-runner > ~/docker/image.tar
	docker save codewars/rust-runner > ~/docker/image.tar
	docker save codewars/dart-runner > ~/docker/image.tar
	docker save codewars/crystal-runner > ~/docker/image.tar
	docker save codewars/julia-runner > ~/docker/image.tar
