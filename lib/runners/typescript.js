var shovel = require('../shovel'),
    util = require('../util'),
    exec = require('child_process').exec;

module.exports.run = function run(opts, cb) {
  var dir = opts.dir;
  shovel.start(opts, cb, {
    solutionOnly: function(runCode, fail) {
      // TODO: Support Setup Code

      var solutionFile = util.codeWriteSync('typescript', opts.solution, dir, 'solution.ts', true);
      exec('tsc ' + solutionFile + ' --module commonjs', function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        runCode({name: 'node', args: [solutionFile.replace('.ts', '.js')]});
      });
    },
    testIntegration: function(runCode, fail) {
      switch (opts.testFramework) {
        case 'mocha':
        case 'mocha_bdd':
          return prepareMocha(opts, 'bdd', runCode, fail);
        case 'mocha_tdd':
          return prepareMocha(opts, 'tdd', runCode, fail);
        default:
          throw 'Test framework is not supported';
      }
    },
    sanitizeStdErr: function(error) {
      error = error || '';
      return error.replace(/(\()?\/codewars\/[(\w\/-\d.js:) ;]*/g, '')
                .replace(/( Object. )?[\(]?\[eval\][-:\w\d\)]* at/g, '')
                .replace(/Module._compile.*/g, '')
                .replace('Object.Test.handleError ', '')
                .replace('  ', ' ');
    },
    sanitizeStdOut: function(stdout) {
      return this.sanitizeStdErr(stdout);
    }
  });

  function prepareMocha(opts, interfaceType, runCode, fail) {
    var code = opts.setup ? `${opts.setup}\n${opts.solution}` : opts.solution;

    var codeFile = util.codeWriteSync('typescript', code, null, 'solution.ts', true);

    exec('tsc --module commonjs ' + codeFile, function(error, stdout, stderr) {
      if (error) return fail(error, stdout, stderr);

      var specFile = util.codeWriteSync('typescript', opts.fixture, null, 'spec.ts', true);

      exec('tsc --module commonjs ' + specFile, function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        specFile = specFile.replace('.ts', '.js');
        runCode({name: 'mocha', 'args': ['-t', opts.timeout || 7000, '-u', interfaceType, '-R', 'mocha-reporter', specFile]});
      });
    });
  }

};
