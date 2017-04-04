'use strict';

const shovel = require('../shovel');
const util = require('../util');
const exec = require('child_process').exec;

let usingTests = false;

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(runCode, fail) {
      usingTests = false;

      const code = `${opts.setup ? opts.setup+'\n' : ''}${opts.code}`;
      util.codeWriteSync('rust', code, opts.dir, 'main.rs', true);

      _compile((err,stdout,stderr) => {
        if (err) return fail({stdout, stderr});

        // Run
        runCode({
          name: `./main`,
          options: {
            cwd: opts.dir
          }
        });
      });
    },
    testIntegration: function(runCode, fail) {
      usingTests = true;

      if (opts.testFramework != 'rust')
        throw 'Test framework is not supported';

      const code = `${opts.setup ? opts.setup+'\n' : ''}${opts.code}\n${opts.fixture}`;
      util.codeWriteSync('rust', code, opts.dir, 'main.rs', true);

      _compile((err,stdout,stderr) => {
        if (err) return fail({stdout, stderr});

        // Run
        runCode({
          name: `./main`,
          options: {
            cwd: opts.dir
          }
        });
      });
    },
    sanitizeStdOut: function(stdout) {
      if (opts.fixture) return _formatOut(stdout);
      return stdout;
    },
    transformBuffer: function(buffer) {
      if (opts.testFramework == 'rust') {
        // if tests failed then just output the entire raw test spec results so that the full details can be viewed
        if (!buffer.stderr && buffer.stdout.indexOf('FAILED') > 0 && buffer.stdout.indexOf('failures:') > 0) {
          buffer.stderr = buffer.stdout.substr(buffer.stdout.indexOf('failures:') + 10).replace("note: Run with `RUST_BACKTRACE=1` for a backtrace.", '');
          // trim after the first failures section
          buffer.stderr = "Failure Info:\n" + buffer.stderr.substr(0, buffer.stderr.indexOf('failures:'));
          if (opts.setup) {
            buffer.stderr += "\nNOTE: Line numbers reported within errors will not match up exactly to those shown within your editors due to concatenation.";
          }
        }
      }
    }
  });

  // Initalizes the Rust dir via cargo
  const _compile = function(cb) {
    exec(`rustc main.rs ${usingTests ? '--test' : ''}`,{cwd: opts.dir}, cb);
  };

  const _formatOut = function(stdout) {
    let output = '';
    let tests = stdout.split(/\n/gm).filter(v => !v.search(/^test.*(?:ok|FAILED)$/));

    for (let test of tests) output += _parseTest(test);

    return output;
  };

  const _parseTest = function(test) {
    let result = test.split(' ');
    let out = `<DESCRIBE::>${result[1]}\n`;
    out += result[3] != 'FAILED' ? `<PASSED::>Test Passed\n` : `<FAILED::>Test Failed\n`;
    return out;
  };
};


