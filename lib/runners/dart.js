const shovel = require('../shovel');
const util = require('../util');
const config = require('../config');
const temp = require('temp').track();

const dir = '/tmp/dart';

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(run) {
      var file = _createSolution(opts);

      // Run
      run({
        name: 'dart',
        args: [file]
      });
    },
    testIntegration: function(run) {
      // test is the only one supported right now...
      if (opts.testFramework != 'test')
        throw 'Test framework is not supported';

      // Create the spec test
      var file = _createSpec(opts);

      // Run
      run({
        name: 'pub',
        args: ['run', 'test', file]
      });

    }
  });
};

// Create the solution file
var _createSolution = function(opts) {
  // Get generated filename (TODO: Fix)
  var file = temp.path({suffix: '.dart'}).split('/').pop();

  var code = opts.setup ? opts.setup + opts.code : opts.code;

  return util.codeWriteSync('dart', code, dir, file);
}

// Create the spec
var _createSpec = function(opts) {
  // Get generated filename (TODO: Fix)
  var file = temp.path({suffix: '.dart'}).split('/').pop();

  var code = `
    import 'dart:async';
    import 'package:test/test.dart';
    import 'package:matcher/matcher.dart';

    ${opts.setup}
    ${opts.code}

    main() {
      ${opts.fixture}
    }
  `;

  return util.codeWriteSync('dart', code, dir, file);
}
