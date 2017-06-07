"use strict";

const exec = require('child_process').exec;
const writeFileSync = require('fs').writeFileSync;

const shovel = require('../shovel');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly(runCode, fail) {
      const dir = '/home/codewarrior';
      writeFileSync('/home/codewarrior/code.d', opts.solution);
      exec('dmd -unittest code.d', {
        cwd: dir,
      }, function(stdout, stderr, err) {
        if (err) return fail(err, stdout, stderr);
        runCode({
          name: './code',
          options: {
            cwd: dir
          }
        });
      });
    },

    testIntegration(runCode, fail) {
      throw new Error('Test integration is not supported');
    }
  });
};
