# codewars-runner

This project is both a command-line utility and server, used by [Codewars](http://www.codewars.com) to execute small sets of code within various languages, using various testing frameworks.

You can run `node run --help` to view information about which arguments are supported.

## Purpose

The purpose of this project is to provide a low level ability to run 'Kata'. It provides a mechanism for executing different configurations of code using various languages and testing frameworks.

Docker can be utilized in order to sandbox code execution. A server is provided that can accept 'run' requests and execute them within a Docker container.

## Supported Languages and Testing Frameworks

- JavaScript
	- mocha (coming soon...)
	- codewars

- CoffeeScript
	- mocha (coming soon...)
	- codewars

- Ruby
	- rspec
	- codewars

- Python
	- unittest
	- codewars

- Java
	- junit

- C#
	- nunit (coming soon...)
	- codewars

- Haskell
	- [hspec](http://hspec.github.io)

- Clojure
	- [clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)

- Julia
	- [FactCheck](https://github.com/zachallaun/FactCheck.jl#usage)

- Erlang
    - (coming soon...)
	
## Examples:

### Basic Usage

The CLI capabilities can be used without having to run Docker. As long as you have the language and frameworks installed on your host machine, you will be able to run the CLI tool standalone.

If you just wanted to execute some code and return its output, you can do so like the following:

#### Ruby 

	node run -l ruby -c 'puts 123'

#### JavaScript

	node run -l js -c 'console.log(123)'

#### Python

	node run -l py -c 'print 123'
  
#### Haskell

	node run -l hs -c 'main = putStrLn "123"'
  
#### Clojure

	node run -l clj -c '(println 123)'`

#### Julia

	node run -l jl -c 'println(123)'

#### Erlang

	node run -l erl -c 'io:fwrite("123\n"), init:stop().'

### Kata Usage

You can also provide a test fixture to be run along with the code. The output returned is the same output that is parsed by Codewars to render its kata output.

#### Ruby

	node run -l ruby -c 'a = 1' -f 'Test.expect a == 1'


#### JavaScript

	node run -l js -c 'a = 1' -f 'Test.expect(a == 1)'

#### Python

	node run -l py -c 'a = 1' -f 'test.expect(a == 1)'


## Docker

#### Pull

	docker pull codewars/codewars-runner

#### Unit testing

	docker run -i --entrypoint mocha codewars/codewars-runner test/*
	
#### Run Help

	docker run --rm codewars/codewars-runner --help
	
#### Run Ruby Kata

	docker run --rm codewars/codewars-runner run -l ruby -c 'a = 1' -f 'Test.expect a == 1'

	
#### Run JavaScript Kata

	docker run --rm codewars/codewars-runner run -l js -c 'a = 1' -f 'Test.expect(a == 1)'

	
#### Run Python Kata

	docker run --rm codewars/codewars-runner run -l py -c 'a = 1' -f 'test.expect(a == 1)'

	
### Server

You can run a server which wraps the CLI and runs it within a Docker container. If you have Docker installed on your machine and the Codewars image built, all you need to do to start the server is run `node server`. You can then make posts requests to `localhost:8080/run` and provide the same arguments that you would for the CLI tool.

### Vagrant

> Version 1.6.3 or higher is required.

 A fully working environment is provided via Vagrant. These steps will get a working server running:
 
	vagrant up
 	vagrant ssh
 	cd /vagrant
 	node build
 	supervisor server.js
	
 You should now have a fully working server with Docker support. You can access the server using `localhost:8080`. You can post requests to `/run` with the same arguments that the CLI accepts.

### Notes about image versioning

The Docker images used by server.js are tagged within a version number. Some utilities have been provided to make it easier to manage versioned images.

#### build.js

Simply run `node build` to build the latest versioned image

### Droplet setup

- Grab the files

	  cd /

	  git clone https://github.com/entrefuse/codewars-runner
	  cd /codewars-runner/setup

- Create a non-root user and give it permissions

	  sh create-user.sh

- Provision the machine

	  sh provision.sh

- Set up the production environment

	  sh prod.sh

- Set up the hourly docker restart

	  bash restarer.sh
