'use strict'

const shovel = require('../shovel');
const util = require('../util');
const config = require('../config');
const fs = require('fs');
const temp = require('temp').track();

const dir = '/tmp/dart';

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(run) {
      _createSolution(opts).then(file => {
        // Run
        run({
          name: 'dart',
          args: [file],
          options: {
            cwd:dir
          }
        });
      });
    },
    testIntegration: function(run) {
      // test is the only one supported right now...
      if (opts.testFramework != 'test')
        throw 'Test framework is not supported';

      // Create the spec test
      _createSpec(opts).then(file => {
        // Run
        run({
          name: 'pub',
          args: ['run', 'test', file],
          options: {
            cwd:dir
          }
        });
      });
    }
  });
};

// Create the solution file
var _createSolution = function(opts) {
  // Set the code, _block() will stop user adding imports
  // that are not provided in setup
  let code = `
    ${opts.setup ? opts.setup : ''}
    _block() {}
    ${opts.code}
  `;

  return _writeCode(code);
}

// Create the spec
var _createSpec = function(opts) {
  // Includes test suites
  let code = `
    import 'package:test/test.dart';
    import 'package:matcher/matcher.dart';
    ${opts.setup ? opts.setup : ''}

    _block() {}

    ${opts.code}

    main() {
      ${opts.fixture}
    }
  `;

  return _writeCode(code);
}

// Helper to allow creating tracked temp file in specific dir
// which has the pubspec.yaml
var _writeCode = function(code) {
  let affix = {
    dir: dir,
    suffix: '.dart'
  }

  return new Promise((resolve, reject) => {
    temp.open(affix, (err, info) => {
      if (err) reject(err);
      else {
        // Write & close
        fs.write(info.fd, code, err => reject(err));
        fs.close(info.fd, err => reject(err));

        resolve(info.path);
      }
    })
  });
}
