# Environment

There are currently two SQL environments available:

## SQLite3

There is a basic SQLite3 environment which will be most performant when doing simple queries

## Postgre 9.6

For more advanced queries, PostgreSQL 9.6 is available. 

# Test Environment

SQL is tested using Ruby and RSpec, leveraging the Sequel gem to communicate with the choosen database environment.
See the Ruby environment documentation for more details about what gems are available.

The sandbox environment will timeout the code within 14 seconds.
 
> For more information, view the [docker file](https://github.com/Codewars/codewars-runner-cli/blob/master/docker/ruby.docker)
