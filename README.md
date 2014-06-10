# codewars-runner

This project is both a command-line utility and server, used by [Codewars](http://www.codewars.com) to execute small sets of code within various languages, using various testing frameworks.

You can run `node run --help` to view information about which arguments are supported.

## Purpose

The purpose of this project is to provide a low level ability to run "Kata". It utilizes Docker containers in order to sandbox the code. Each time the code is ran, it is executed within a new Docker container.

Through this project, the Codewars community can contribute support for new languages and frameworks to be supported by codewars. 

## Supported Languages and Testing Frameworks

- JavaScript
    - mocha (comming soon...)
    - codewars

- CoffeeScript
    - mocha (comming soon...)
    - codewars

- Ruby
    - rspec (comming soon...)
    - codewars

- Python
    - unittest (comming soon...)
    - codewars (comming soon...)

## Examples:

### Basic Usage
If you just wanted to execute some code and return its output, you can do so like the following:

#### Ruby
```
node run -l ruby -c "puts 123"
```

#### JavaScript
```
node run -l js -c "console.log(123)"
```

### Kata Usage
You can also provide a test fixture to be ran along with the code. The output returned is the same output that is parsed
by Codewars to render its kata output.

#### Ruby
```
node run -l ruby -c "a = 1" -f "Test.expect a == 1"
```

#### JavaScript
```
node run -l js -c "a = 1" -f "Test.expect(a == 1)"
```

## Docker

The cli now supports running inside a docker container for better isolation.

#### Build
```
docker build -t codewars/cli-runner .
```

#### Run Help
```
docker run --rm codewars/cli-runner --help
```

#### Run JavaScript Kata
```
docker run --rm codewars/cli-runner -l js -c "a = 1" -f "Test.expect(a == 1)"
```

#### Run Ruby Kata
```
docker run --rm codewars/cli-runner -l ruby -c "a = 1" -f "Test.expect a == 1"
```

### Server

You can run a server which wraps the CLI and runs it within a Docker container. If you have Docker installed on your machine and the Codewars image built, all you need to do to start the server is run `node server`. You can then make posts requests to `localhost:8080/run` and provide the same arguments that you would for the CLI tool. 

### Vagrant

> Version 1.6.3 or higher is required.

 A fully working environment is provided via Vagrant. These steps will get a working server running:
 ```
 vagrant up
 vagrant ssh
 supervisor /vagrant/server.js
 ```

 You should now have a fully working server with Docker support. You can access the server using `localhost:8080'.
