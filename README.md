# About

[![Build Status](https://travis-ci.org/Codewars/codewars-runner-cli.svg?branch=master)](https://travis-ci.org/Codewars/codewars-runner-cli)

This project is used by [Codewars](http://www.codewars.com) and [Strive](https://www.strive.co) to execute small sets of code within various languages, using various testing frameworks.
Each time code is run, it is executed within a Docker container in order to secure unsafe code execution.  

All execution is done within Docker, with a Node CLI app contained within each container
that manages the code execution for that specific language environment and returns the result via stdout. 

## Contributions

This project has been open-sourced so that the Codewars and Strive communites can contribute support for new languages and
frameworks. See the [Language Support Status section](#user-content-language-support-status) for more information regarding which languages are currently supported
and where their Codewars/Strive support has been added.
 
## Basic Usage

Within each Docker image, there is a copy of the Node executable and a `run` script. That script accepts multiple options
for executing code. For example to run a simple javascript script which would output `2`:

```
# this is how you run the Node CLI. For this to work you would have to bash into the correct Docker image
node run -l javascript -c "console.log(1+1)"
```

Because everything runs inside of Docker, you would normally not run Node directly from your host but instead via a Docker run command.
To do this, you would either bash into the correct Docker image like so:

```
# direct Docker call:
docker run --rm -it codewars/node-runner bash

# alternatively you can use the provided Docker Compose configuration:
docker-compose run node-runner
```

Or you could choose to execute the code outside of Docker by creating a container that will remove itself after it executes: 

```
# direct Docker call:
docker run --rm codewars/node-runner run -l javascript -c "console.log('I ran inside of Docker using NodeJS')"
docker run --rm codewars/ruby-runner run -l ruby -c "puts 'I ran inside of Docker using Ruby'"

# alternatively you can use the provided Docker Compose configuration:
docker-compose run javascript -c "console.log('I ran inside of Docker using NodeJS')"
docker-compose run ruby -c "puts 'I ran inside of Docker using Ruby'"
```

### Integrated Test Suites
The most significant aspect of this project is that integrated test support is built in to many languages. This is how
Codewars and Strive work, instead of testing STDOUT of a program, the executed code is tested using traditional
code testing methods, with unit-tests and language-specific testing frameworks. 

Here is a very simple example of running tests using the simplified Codewars testing framework. 

```
# manually running docker
docker run --rm codewars/node-runner run -l javascript -c "var a = 1;" -t cw -f "Test.assertEquals(a, 1)"

# using docker compose
docker-compose run javascript -c "var a = 1;" -t cw -f "Test.assertEquals(a, 1)"
```

Which would output `<PASSED::>Test Passed: Value == 1` to STDOUT.

## Language Support Status 

Many languages are currently supported in various states of completeness. This list tries to keep track of each.

**Legend:** `!!!` = Failing Specs, `???` = Status is unknown, `*` = Any

| Language       | Version       | Basic Run    | Test Integration | Codewars     | Strive         | Docker Image   | Examples     | Notes                                                                   |
|----------------|---------------|--------------|------------------|--------------|----------------|----------------|--------------|-------------------------------------------------------------------------|
| Assembly (GAS) |               | !!!          |                  |              |                | systems-runner |              | Travis is failing, tests pass locally                                   |
| Bash           |               | ✓            |                  | Kumite Only  |                | *              |              |                                                                         |
| C              |               | !!!          | !!!              |              |                | systems-runner |              | Travis is failing, tests pass locally                                   |
| Clojure        | 1.6.0         | ✓            | clojure.test     | clojure.test | clojure.test   | func-runner    | clojure.test |                                                                         |
| CoffeeScript   | 1.10.0        | ✓            | cw-2             | cw-2         | cw-2           | node-runner    | cw-2         |                                                                         |
| C++            |               | ✓            | !!!              |              |                | systems-runner |              | Failing specs, systems image has also been temporarily removed          |
| C#             | Mono 4.2.3    | ✓            | nunit            | nunit        | nunit          | dotnet-runner  | nunit        |                                                                         |
| Elixir         | 1.2.4         | ✓            | exunit           | exunit       |                | erlang-runner  |              |                                                                         |
| Erlang         |               | ✓            |                  |              |                | erlang-runner  |              |                                                                         |
| F#             | 4.0           | ✓            |                  | Kumite Only  |                | dotnet-runner  |              |                                                                         |
| Go             | 1.3.1         | ✓            |                  | Kumite Only  |                | alt-runner     |              |                                                                         |
| Groovy         |               | ✓            |                  | Kumite Only  |                | jvm-runner     |              |                                                                         |
| Haskell        | 7.6.3         | ✓            | hspec!!!         | hspec        | hspec          | haskell-runner | hspec        | An older version is running on CW & Strive that is fully functional     |
| Java           | 1.8.0_91      | ✓            | junit            | Yes          | Yes            | jvm-runner     | junit        |                                                                         |
| JavaScript     | 0.10.33       | ✓            | cw-2, mocha      | cw-2         | cw-2, mocha    | node-runner    | cw-2         |                                                                         |
| JavaScript     | 0.10.33/Babel | ✓            | cw-2, mocha      | cw-2         | cw-2, mocha    | node-runner    | cw-2         |                                                                         |
| JavaScript     | 6.0.0         | ✓            | cw-2, mocha      | cw-2         | cw-2, mocha    | node-runner    | cw-2         |                                                                         |
| JavaScript     | 6.0.0/Babel   | ✓            | cw-2, mocha      | cw-2         | cw-2, mocha    | node-runner    | cw-2         |                                                                         |
| Julia          |               | ✓!!!         | !!!              |              |                |                |              |                                                                         |
| Lisp           |               | ✓            |                  | Kumite Only  |                | func-runner    |              |                                                                         |
| Lua            |               | ✓            |                  | Kumite Only  |                | alt-runner     |              |                                                                         |
| ObjC           |               | ???          | ???              |              |                |                |              |                                                                         |
| OCAML          |               | ✓            |                  | Kumite Only  |                | func-runner    |              |                                                                         |
| Perl           |               | ✓            |                  | Kumite Only  |                | *              |              |                                                                         |
| Php            |               | ✓            |                  | Kumite Only  |                | alt-runner     |              |                                                                         |
| Python         | 2.7.6         | ✓            | cw-2, unittest   | cw-2         | cw-2, unittest | python-runner  | cw-2         |                                                                         |
| Python         | 3.4.3         | ✓            | cw-2, unittest   |              | cw-2, unittest | python-runner  | cw-2         |                                                                         |
| R              |               | ✓            |                  |              |                | alt-runner     |              |                                                                         |
| Racket         |               | ✓            |                  | Kumite Only  |                | func-runner    |              |                                                                         |
| Ruby           | 2.3.0         | ✓            | cw-2, rspec      | cw-2         | cw-2, rspec    | ruby-runner    | cw-2         |                                                                         |
| Rust           |               | ✓            |                  |              |                |                |              |                                                                         |
| Scala          | 2.11.2        | ✓            |                  | Kumite Only  |                | jvm-runner     |              |                                                                         |
| Swift          |               | ???          | ???              |              |                |                |              | Current contribution designed for OSX, need to move to OS linux version |
| TypeScript     | 1.8.10        | ✓            | mocha            | mocha        |                | node-runner    |              | TypeScript utilizes `require` instead of concatenating files            |

## Setup

You should have Docker installed, if not do that first. Before you can run any of the code
environments you will need to build the proper Docker image. To get started lets work with the
node image.

Run `make node` to build the base and node images. This will take a few minutes.

Once you image is built, you can create a container to work within it. Doing this means you do not
have to worry about having any of the project dependencies loaded directly on your machine.

Run the following command:

```
docker run \
    -it \
    --rm \
    --entrypoint bash \
    -v $(pwd)/lib:/runner/lib \
    -v $(pwd)/examples:/runner/examples \
    -v $(pwd)/frameworks:/runner/frameworks \
    -v $(pwd)/test:/runner/test \
    codewars/node-runner
```

This will create a new container and send you into the instance with your project's lib and test directories mounted
as volumes. Mounting as a volume allows you to change files on your local machine and have those changes available to you
from within the container.

**Notice**: We did not mount the entire directory because that would overwrite things such as your node_modules directory. If you need
to update these you should `make {image_you_want_to_update}` the image to ensure you are always testing against the correct packages.

### Docker Compose
We mentioned before that you also have the option of using Docker Compose to run the CLI tool. We have setup the `docker-compose.yml`
file to provide very useful pre-configured services for making development easier. Instead of having to issue the long command
mentioned above, you can simply run `docker-compose run node_runner` to bash into a fresh container with your local volumes already mounted.
 
All of the docker compose services are setup to mount volumes to make development easier, so that is the recommended way of 
interacting with the codebase. You should note though that the compose file is unable to build images due to how
the directory structure is laid out, so you have to first `make {runner_name}` the image before you can run it. Otherwise
it will pull down the latest copy from Docker Hub.

### Running Tests

Once you are in the Docker image, you can run tests as a part of your development flow. For example:

```
# inside of container
mocha test/runners/typescript_spec.js

# or from outside the container
docker-compose run typescript_test
```

## Test Suite Output Format

A custom and very basic format is used for sending data out of the CLI tool. All formatted data is returned via STDOUT. 
If you do nothing but write normal strings to STDOUT, then codewars.com will display each line as you would expect, unformatted.

A small subset of commands is supported that can be used to format output. They are:

- `<DESCRIBE::>`
- `<IT::>`
- `<PASSED::>`
- `<FAILED::>`
- `<ERROR::>`
- `<COMPLETEDIN::>`

Prefixing a new line with these commands will cause that line to be formatted. 
Since each new STDOUT line is considered a new peace of data, if you wish to format multiple lines as one 
item (such as a multi line "passed" message), then you must replace all \n line feed characters with the `<:LF:>` token.

For example, in Ruby, if you wanted to write a multi-line passed message:

```ruby
def passed(msg)
  puts "<PASSED::>#{msg.gsub("\n", "<:LF:>")}"
end
```

### Nested Describes

Some test frameworks support nested levels of describes. In order for our output to support multiple levels, 
you must also use the `<COMPLETEDIN::>` token, which acts as a terminator for the current item. This should be used 
to terminate both `<DESCRIBE::>` and `<IT::>` statements.

The following is a full example of what the output might look like, that supports nested describes:

```
<DESCRIBE::>Foo
<IT::>It should return a string
<PASSED::>Test Passed
<COMPLETEDIN::>23
<IT::>It should return "foo" 
This is some direct output (i.e. console.log("..."))
<FAILED::>Expected "foo" but instead got ""
<COMPLETEDIN::>10
<DESCRIBE::>This is a nested describe
<IT::>Should not be null
<PASSED::>Test Passed
<COMPLETEDIN::>20
<COMPLETEDIN::>22
<COMPLETEDIN::>100
```

> Notice how there are 3 `<COMPLETEDIN::>` statements at the end. The first one completes the last IT
statement, the 2nd completes the nested DESCRIBE and the 3rd completes the top level "Foo" DESCRIBE.

#### <COMPLETEDIN::> Details

The value of COMPLETEDIN should be time spent executing the related statement, in milliseconds. It is not required
to support time. `<COMPLETEDIN::>` is valid on its own, and in that case it is only used to terminate the current statement. 

### Why the custom format?

Getting different test suites in different languages to all play together with the same format can be tricky. In many cases, 
customizing the test suite output is very limited (sometimes requiring hacking). Because of this, using formats such as 
XML and JSON are complicated because its not always possibly to correctly close out the data format when a program raises an exception. 

The format choosen was originally done so that at any point in time the program could exit while still having readable data.
Other formats, such as TAP (Test Anything Protocol) could also be an option. However another requirement that we had when
designing the format was to have it be incredibly simple yet flexible, so that Codewars.com could support more than simply
outputing test results. With the current format there is nothing stopping you from outputing HTML, JS canvas code, etc in order
to create a rich and even interactive test result output.
  
  
## How to add a new language

> Note: These steps all assume you are adding a completely new language to the project. Many languages are currently in an incomplete
state so not all steps may be needed in your case

1. Install the language and its related packages on one of the Docker images. We have grouped many of the Docker images
together so if that grouping makes sense then add it there, otherwise you will need to create a new docker image within
the docker folder
1. Add the newly created docker file to the Make and docker-compose files (if applicable)
1. Add a new runner script within `lib/runners`. More details about this script later on.
1. Add a new runner spec within `test/runners`. You will also need to add the runner spec to the Docker image so that
it is tested as a part of the build process.
1. Add a new examples yml file within the `examples` folder. These are the code examples that are used on Codewars.com
when a user clicks the "Insert Example" button within the kata editor. There is also a helper available for running your examples
as a part of the test suite.


### Runner Script

The runner script is responsible for outputing a `run` method. This method utilizes the shovel helper which will handle
all of the inter-process communication for you. The shovel config accepts strategies to determine how to handle running
the code based off of the options passed in. There are currently two types of strategies. 

- `solutionOnly`: Code is simply executed and its STDOUT returned. There is no test integration.
- `testIntegration`: Code is executed within a configurable test suite and the test output is returned via STDOUT.

Each of these strategies is passed in a `run` method which is used to ultimately execute the final command.
 
## TODO

### Paradigm Shift

There is currently no way of handling language/package versioning well. This is largely caused by the Node CLI tool having to be 
baked in to the Docker container. A more ideal solution would involve keeping the CLI outside of Docker (or within its own sibling container)
and communicating to language specific containers. This would allow us to easily swap out container versions that have no
dependencies on the CLI codebase. This would involve having to utilize the `docker cp` command to copy
files to the docker container in order to be compiled. Upgrading to this newer paradigm is the next big phase of this project. 

### Ability to send input parameters and return data

This would likely involve a new shovel strategy. One where we can pass in parameters to the code and expect a specific format to
be returned (like JSON). This would allow Codewars to eventually support dynamic APIs leveraging user submitted code - opening
up new possibilites in what the community could build together as a social experiment.
