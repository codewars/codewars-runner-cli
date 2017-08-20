"use strict";

const path = require('path');

const temp = require('temp').track();
const fs = require('fs-extra');

module.exports = {
  solutionOnly(opts, runCode) {
    const dir = temp.mkdirSync('perl');
    const file = path.join(dir, 'solution.pl');
    fs.outputFileSync(file, opts.solution);
    runCode({name: 'perl', 'args': [file]});
  },
  testIntegration(opts, runCode) {
    throw new Error('Test framework is not supported');
  }
};
