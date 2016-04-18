HOSTNAME=codewars
# NOTE: systems was removed do to failing Travis issues. Must debug those issues before systems can be put back in
CONTAINERS=dotnet jvm node python ruby alt func erlang

ALL_CONTAINERS=${CONTAINERS} base

.PHONY: ${ALL_CONTAINERS} clean docker_rm docker_rmi

all: ${CONTAINERS}

base:
	cp docker/$@.docker ./Dockerfile
	docker build -t $(HOSTNAME)/$@-runner .

${CONTAINERS}: base
	cp docker/$@.docker ./Dockerfile
	docker build -t $(HOSTNAME)/$@-runner .

# Push docker containers to registry
push_to_registry:
	docker push $(HOSTNAME)/base-runner
	echo $(patsubst %, $(HOSTNAME)/%-runner, $(CONTAINERS)) | xargs -n 1 docker push 

# Remove docker processes that have exited cleanly
docker_rm_exited:
	[ ! -n "$(shell docker ps -a | grep Exit | cut -d ' ' -f 1)" ] || echo $(shell docker ps -a | grep Exit | cut -d ' ' -f 1) | xargs -n 1 docker rm -f

# Cleanup temporary images that are no longer used
docker_rmi_temporary:
	docker rmi $(docker images -q -f dangling=true)

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
	docker pull codewars/ruby-runner
	docker pull codewars/node-runner
	docker pull codewars/python-runner
	docker pull codewars/dotnet-runner
	docker pull codewars/jvm-runner
	docker pull codewars/systems-runner
	docker pull codewars/func-runner
	docker pull codewars/erlang-runner
	docker pull codewars/alt-runner
