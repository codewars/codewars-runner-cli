# Environment

Code is executed within a Dockerized Ubuntu 14.04 container. 

## Language Versions

Python 2.7.6 and 3.4.3 are supported.

## Testing Frameworks

- Codewars (custom framework used for authoring and testing kata on codewars.com)
- UnitTest (currently only available on Qualified.io and Codewars.com kumite) 

## Installed Packages

- scikit-learn
- tensorflow
- numpy
- scipy
- pandas
- pymongo
- redis

## Installed Services

- sqlite
- redis-server
- mongodb

> For more information, view the [docker file](https://github.com/Codewars/codewars-runner-cli/blob/master/docker/python.docker)