# Environment

Code is executed within a Dockerized Ubuntu 14.04 container.

## Languages

- Clojure 1.8

## Loaded Dependencies

All languages share the following:

- cheshire 5.3.1
- junit 4.11
- environ 0.5.0

Clojure uses `clojure.test` as its test framework.

# Timeout

The sandbox environment will timeout the code within 11 seconds. In the future, as Codewars expands its abilities, this limitation may be increased in order to handle more complex kata/code challenges.

> For more information, view the [docker file](https://github.com/Codewars/codewars-runner-cli/blob/master/docker/jvm.docker)
