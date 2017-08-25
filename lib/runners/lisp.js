"use strict";

const path = require('path');

const fs = require('fs-extra');
const temp = require('temp').track();

module.exports = {
  solutionOnly(opts, runCode) {
    const dir = temp.mkdirSync('lisp');
    const args = [
      '--noinform', // Disable banner
      '--disable-ldb', // Disable the low-level debugger
      '--lose-on-corruption', // Don't try to recover
      '--non-interactive', // No REPL
    ];
    if (opts.setup) {
      const setupFile = path.join(dir, 'setup.lisp');
      fs.outputFileSync(setupFile, opts.setup);
      args.push('--load', setupFile);
    }
    args.push('--eval', opts.solution);
    runCode({name: 'sbcl', args: args});
  },
  testIntegration(opts, runCode) {
    throw new Error('Test framework is not supported');
  }
};
