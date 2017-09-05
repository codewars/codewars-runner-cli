# Environment

Code is executed within a Dockerized Ubuntu 14.04 container. 

## JavaScript

Node `0.10.33`, `6.11.0`, and `8.1.3` are currently supported, each with the option of transpiling using Babel JS. Node 6 without babel is the recommended option due to it supporting most ES2015 features natively, and because it does not incur the additional transpilation performance penalty that Babel JS causes.
 
Node runtime versions can be specified in a forward-compatible manner using the following formats:
 
- `0.10.x` or `0.10.x/babel`: Run on node 0.10 with or without Babel transpilation
- `6.x` or `6.x/babel`: The same for Node 6
- `8.x` or `8.x/babel`: The same for Node 8

If no `languageVersion` is specified, the default is `6.x`.

### 0.10.x Babel Plugins

The following presets are included:
- stage-1
- react
- es2015

The following plugins are included:
- angular2-annotations
- transform-decorators-legacy
- transform-class-properties
- transform-flow-strip-types

### 6.x/8.x Babel Plugins

The following presets are included:
- stage-1
- node5
- react

The following plugins are included:
- angular2-annotations
- transform-decorators-legacy
- transform-class-properties
- transform-flow-strip-types

### How different JS code sections are treated

The solution, test fixture and optional setup/preloaded code are all combined into a single file. The test fixture is wrapped within its own clojure.

## CoffeeScript

CoffeeScript 1.10.0 is supported, executed within Node 6.0.

### How different Coffee code sections are treated

The solution, test fixture and optional setup/preloaded code are all combined into a single file. The test fixture is wrapped within its own clojure.

## TypeScript

Typescript 2.4 is supported. The Mocha test framework and Karma+Mocha are available for testing.
 
The following typings are available:

- /runner/typings/baconjs/index.d.ts
- /runner/typings/chai/index.d.ts
- /runner/typings/lodash/index.d.ts
- /runner/typings/mocha/index.d.ts
- /runner/typings/mocha-node/index.d.ts
- /runner/typings/mongoose/index.d.ts
- /runner/typings/node/index.d.ts
- /runner/typings/react/index.d.ts
- /runner/typings/redis/index.d.ts
- /runner/typings/rx/index.d.ts

### How different TypeScript code sections are treated

The solution and optional setup/preloaded code are combined into one "solution" file. The test fixture is its own file, which automatically requires the solution file as `solution`. 

# Timeout

The sandbox environment will timeout the code within 7 seconds. In the future, as Codewars expands its abilities, this limitation may be increased in order to handle more complex kata/code challenges. 

# Node Packages

The following packages have been installed:

- async
- baconjs
- base64-js
- bluebird
- brain
- brfs
- buffer-equal
- chai
- chai-change
- chai-factories
- chai-spies
- chai-subset
- dclassify
- deku
- elasticsearch
- esprima
- expect
- falafel
- graph-paths 
- immutable
- js-yaml
- karma
- karma-chai
- karma-mocha
- karma-phantomjs-launcher
- karma-typescript
- karma-typescript-angular2-transform
- karma-typescript-es6-transform
- lodash
- mocha
- mongodb
- mongoose
- natural
- phantomjs
- quickcheck
- react
- react-dom
- redis
- rx
- should
- sqlite3
- support for Angular v4, including:
    - @angular/common
    - @angular/compiler
    - @angular/core
    - @angular/forms
    - @angular/http
    - @angular/platform-browser
    - @angular/platform-browser-dynamic
    - @angular/router
    - core-js
    - rxjs
    - zone.js

# Installed Services

- sqlite
- redis-server
- mongodb

> For more information, view the [docker file](https://github.com/Codewars/codewars-runner-cli/blob/master/docker/node.docker)
