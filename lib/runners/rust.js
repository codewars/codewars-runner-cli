'use strict'

const shovel = require('../shovel');
const util = require('../util');
const fs = require('fs');
const path = require('path');
const exec = require('child_process').exec;
const temp = require('temp').track();

const dir = temp.mkdirSync('rust');
let usingTests = false;

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(run) {
      usingTests = false;

      let file = _createSolution(opts);

      _compile((err,stdout,stderr) => {
        if(err) return cb({stdout,stderr});

        // Run
        run({
          name: `./main`,
          options: {
            cwd: dir
          }
        });
      });
    },
    testIntegration: function(run) {
      usingTests = true;

      if (opts.testFramework != 'rust')
        throw 'Test framework is not supported';

      let file = _createSpec(opts);

      _compile((err,stdout,stderr) => {
        if(err) {
          stdout = _formatErr(stderr);
          return cb({stdout,stderr:''});
        }

        // Run
        run({
          name: `./main`,
          options: {
            cwd: dir
          }
        });
      });
    },
    sanitizeStdOut: function(stdout) {
      if (opts.fixture) return _formatOut(stdout)
      else return stdout;
    }
  });
};

// Create the solution file
const  _createSolution = function(opts) {
  // Set the code
  let code = `
    ${opts.setup ? opts.setup : ''}

    ${opts.code}
  `;

  return util.codeWriteSync('rust', code, dir, 'main.rs', true);
}

// Create the spec
const  _createSpec = function(opts) {
  let code = `
    ${opts.setup ? opts.setup : ''}

    ${opts.code}

    ${opts.fixture}
  `;

  return util.codeWriteSync('rust', code, dir, 'main.rs', true);
}

// Initalizes the Rust dir via cargo
const _compile = function(cb) {
  exec(`rustc main.rs ${usingTests ? '--test' : ''}`,{cwd: dir},cb);
}

const _formatOut = function(stdout) {
  let output = '';
  let tests = stdout.split(/\n/gm).filter(v => !v.search(/^test.*(?:ok|FAILED)$/));

  for(let test of tests) output += _parseTest(test);

  return output;
}

const _formatErr = function(stderr) {
  return stderr.split('\n').reduce((prev,curr) => prev += curr.substr(8) + '\n','<ERROR::>');
}

const _parseTest = function(test) {
  let result = test.split(' ');
  let out = `<DESCRIBE::>${result[1]}\n`;
  out += result[3] != 'FAILED' ? `<PASSED::>Test Passed\n` : `<FAILED::>Test Failed\n`;
  return out;
}
