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
    solutionOnly: function(runCode, fail) {
      usingTests = false;

      let file = _createSolution(opts);

      _compile((err,stdout,stderr) => {
        if(err) return fail({stdout, stderr});

        // Run
        runCode({
          name: `./main`,
          options: {
            cwd: dir
          }
        });
      });
    },
    testIntegration: function(runCode, fail) {
      usingTests = true;

      if (opts.testFramework != 'rust')
        throw 'Test framework is not supported';

      let file = _createSpec(opts);

      _compile((err,stdout,stderr) => {
        if (err) return fail({stdout, stderr});

        // Run
        runCode({
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
    },
    transformBuffer: function(buffer) {
      if (opts.testFramework == 'rust') {
        // if tests failed then just output the entire raw test spec results so that the full details can be viewed
        if (!buffer.stderr && buffer.stdout.indexOf('FAILED') > 0 && buffer.stdout.indexOf('failures:') > 0) {
          buffer.stderr = buffer.stdout.substr(buffer.stdout.indexOf('failures:') + 10).replace("note: Run with `RUST_BACKTRACE=1` for a backtrace.", '');
          // trim after the first failures section
          buffer.stderr = "Failure Info:\n" + buffer.stderr.substr(0, buffer.stderr.indexOf('failures:'));
          if (opts.setup) {
            buffer.stderr += "\nNOTE: Line numbers reported within errors will not match up exactly to those shown within your editors due to concatenation."
          }
        }
      }
    }
  });
};

// Create the solution file
const  _createSolution = function(opts) {
  // Set the code
  let code = opts.setup ? `${opts.setup}\n${opts.code}` : opts.code;

  return util.codeWriteSync('rust', code, dir, 'main.rs', true);
}

// Create the spec
const  _createSpec = function(opts) {
  let code = opts.setup ? `${opts.setup}\n${opts.code}` : opts.code;
  code += `\n${opts.fixture}`

  return util.codeWriteSync('rust', code, dir, 'main.rs', true);
}

// Initalizes the Rust dir via cargo
const _compile = function(cb) {
  exec(`rustc main.rs ${usingTests ? '--test' : ''}`,{cwd: dir}, cb);
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
