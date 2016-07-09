'use strict'

const shovel = require('../shovel');
const util = require('../util');
const fs = require('fs');
const path = require('path');
const exec = require('child_process').exec;
// const temp = require('temp');
const temp = require('temp').track();

const dir = temp.mkdirSync('rust');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(run) {
      _init(err => {
        let file = _createSolution(opts);

        // Run
        run({
          name: 'cargo',
          args: ['run'],
          options: {
            cwd: dir
          }
        });
      });
    },
    testIntegration: function(run) {
      // test is the only one supported right now...
      if (opts.testFramework != 'cargo')
        throw 'Test framework is not supported';
        _init(err => {
          let file = _createSpec(opts);

          // Run
          run({
            name: 'cargo',
            args: ['test'],
            options: {
              cwd: dir
            }
          });
        });

    },
    sanitizeStdOut: function(stdout) {
      if (opts.fixture) return _format(stdout)
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

  return util.codeWriteSync('rust', code, `${dir}/src`,'main.rs', true);
}

// Create the spec
const  _createSpec = function(opts) {
  let code = `
    ${opts.setup ? opts.setup : ''}

    ${opts.code}

    ${opts.fixture}
  `;

  return util.codeWriteSync('rust', code, `${dir}/src`,'main.rs', true);
}

// Initalizes the Rust dir via cargo
const _init = function(cb) {
  exec(`cargo init --bin`,{cwd: dir},cb);
}

const _format = function(stdout) {
  let output = '';
  let tests = stdout.split(/\n/gm).filter(v => !v.search(/^test.*(?:ok|FAILED)$/));

  for(let test of tests) output += _parseTest(test);

  return output;
}

const _parseTest = function(test) {
  let result = test.split(' ');
  let out = `<DESCRIBE::>${result[1]}\n`;
  out += result[3] != 'FAILED' ? `<PASSED::>Test Passed\n` : `<FAILED::>Test Failed\n`;
  return out;
}
