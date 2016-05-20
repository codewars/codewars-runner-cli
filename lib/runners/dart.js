const shovel = require('../shovel');
const util = require('../util');
const config = require('../config');
const temp = require('temp');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(run) {
      var dir = 'dart/bin';
      var fileName = 'solution.dart';

      var file = util.codeWriteSync('dart', opts.code, dir, fileName);

      run({
        name: 'dart',
        'args': [file]
      });
    },
    testIntegration: function(run) {
      throw 'Test framework is not supported'
    }
  });
};
