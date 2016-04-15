# runner

This project is both a command-line utility and server, used by [Codewars](http://www.codewars.com) to execute small sets of code within various languages, using various testing frameworks.

You can run `node run --help` to view information about which arguments are supported.

## Purpose

The purpose of this project is to provide a low level ability to run 'Kata'. It provides a mechanism for executing different configurations of code using various languages and testing frameworks.

Docker can be utilized in order to sandbox code execution. A server is provided that can accept 'run' requests and execute them within a Docker container.

## Language Support Status 

Many languages are currently supported in various states of completeness. This list tries to keep track of each.

**Legend:** `!!!` = Failing Specs, `???` = Status is unknown, `*` = Any

| Language     | solutionOnly | testSuite      | Codewars     | Strive         | Docker Image   | Examples     | Notes                                                                   |
|--------------|--------------|----------------|--------------|----------------|----------------|--------------|-------------------------------------------------------------------------|
| Bash         | ✓            |                | Kumite       |                | *              |              |                                                                         |
| C            | ✓            | Failing        | ?            |                | systems-runner |              |                                                                         |
| Clojure      | ✓            | clojure.test   | clojure.test | clojure.test   | func-runner    | clojure.test |                                                                         |
| CoffeeScript | ✓            | cw-2, mocha    | cw-2         | cw-2, mocha    | node-runner    | cw-2         |                                                                         |
| CPP          | ✓            |                |              |                | systems-runner |              |                                                                         |
| C#           | ✓            | nunit          | nunit        | nunit          | dotnet-runner  | nunit        |                                                                         |
| Elixir       | ✓            |                |              |                | erlang-runner  |              |                                                                         |
| Erlang       | ✓            |                |              |                | erlang-runner  |              |                                                                         |
| F#           | ✓            |                | Kumite Only  |                | dotnet-runner  |              |                                                                         |
| Go           | ✓            |                | Kumite Only  |                | alt-runner     |              |                                                                         |
| Groovy       | ✓            |                | Kumite Only  |                | jvm-runner     |              |                                                                         |
| Haskell      | ✓            | hspec!!!       | hspec        | hspec          | func-runner    | hspec        | An older version is running on CW & Strive that is fully functional     |
| Java         | ✓            | junit          | Yes          | Yes            | jvm-runner     | junit        |                                                                         |
| JavaScript   | ✓            | cw-2, mocha    | cw-2         | cw-2, mocha    | node-runner    | cw-2         |                                                                         |
| Julia        | ✓!!!         | Failing        |              |                |                |              |                                                                         |
| Lisp         | ✓            |                | Kumite Only  |                | func-runner    |              |                                                                         |
| Lua          | ✓            |                | Kumite Only  |                | alt-runner     |              |                                                                         |
| ObjC         | ???          | ???            |              |                |                |              |                                                                         |
| OCAML        | ✓            |                | Kumite Only  |                | func-runner    |              |                                                                         |
| Perl         | ✓            |                | Kumite Only  |                | *              |              |                                                                         |
| Php          | ✓            |                | Kumite Only  |                | alt-runner     |              |                                                                         |
| Python 2     | ✓            | cw-2, unittest | cw-2         | cw-2, unittest | python-runner  | cw-2         |                                                                         |
| Python 3     | ✓            | cw-2, unittest |              | cw-2, unittest | python-runner  | cw-2         |                                                                         |
| R            | ✓            |                |              |                | alt-runner     |              |                                                                         |
| Racket       | ✓            |                | Kumite Only  |                | func-runner    |              |                                                                         |
| Ruby         | ✓            | cw-2, rspec    | cw-2         | cw-2, rspec    | ruby-runner    | cw-2         |                                                                         |
| Rust         | ✓            |                |              |                |                |              |                                                                         |
| Scala        | ✓            |                | Kumite Only  |                | jvm-runner     |              |                                                                         |
| Swift        | ???          | ???            |              |                |                |              | Current contribution designed for OSX, need to move to OS linux version |
| TypeScript   | ✓            | mocha          | Kumite Only  |                | node-runner    |              | TypeScript utilizes `require` instead of concatenating files            |


## Setup

You should have Docker installed, if not do that first. Before you can run any of the code
environments you will need to build the proper Docker image. To get started lets work with the
node image.

Run `make base node` to build the base and node images. This will take a few minutes.

Once you image is built, you can create a container to work within it. Doing this means you do not
have to worry about having any of the project dependencies loaded directly on your machine.

Run the following command:

```
docker run -it --rm --entrypoint bash -v $(pwd)/lib:/runner/lib -v $(pwd)/frameworks:/runner/frameworks -v $(pwd)/test:/runner/test codewars/node-runner
```

This will create a new container and send you into the instance with your project's lib and test directories mounted
as volumes. Mounting as a volume allows you to change files on your local machine and have those changes available to you
from within the container.

> We do not mount the entire directory because that would overwrite things such as your node_modules directory. If you need
to update these you should `make node` the image to ensure you are always testing against the correct packages.
