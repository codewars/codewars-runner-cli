"use strict";

const path = require('path');

const fs = require('fs-extra');
const temp = require('temp').track();

module.exports = {
  solutionOnly(opts, run) {
    const dir = temp.mkdirSync('racket');
    const args = ['-l', 'racket/base'];
    if (opts.setup) {
      const setupFile = path.join(dir, 'setup.rkt');
      fs.outputFileSync(setupFile, opts.setup);
      args.push('-t', setupFile);
    }
    args.push('-e', opts.solution);
    run({
      name: 'racket',
      args: args
    });
  },
  testIntegration(opts, run) {
    throw new Error('Test framework is not supported');
  }
};
