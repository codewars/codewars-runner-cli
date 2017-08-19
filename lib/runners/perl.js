"use strict";

const path = require('path');

const temp = require('temp');
const fs = require('fs-extra');

const shovel = require('../shovel');

module.exports.run = function run(opts, cb) {
  temp.track();

  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      const dir = temp.mkdirSync('perl');
      const file = path.join(dir, 'solution.pl');
      fs.outputFileSync(file, opts.solution);
      runCode({name: 'perl', 'args': [file]});
    },
    testIntegration: function(runCode) {
      throw 'Test framework is not supported';
    }
  });
};
