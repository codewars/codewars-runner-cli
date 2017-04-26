var shovel = require('../shovel'),
    util = require('../util'),
    exec = require('child_process').exec,
    path = require('path');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(runCode, fail) {
      var executable = path.join(opts.dir, 'solution'),
          solutionFile = util.codeWriteSync('c', opts.solution, opts.dir, 'solution.c'),
          args = ['clang-3.6', '-std=c11', solutionFile, '-o', executable, '-lm'];
      if (opts.setup) {
        var setupFile = util.codeWriteSync('c', opts.setup, opts.dir, 'setup.c');
        args.push(setupFile);
      }
      exec(args.join(' '), function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        opts.publish('stdout', stdout);
        runCode({'name': executable, 'args': []});
      });
    },
    testIntegration: function(runCode, fail) {
      var executable = path.join(opts.dir, 'solution'),
          solutionFile = util.codeWriteSync('c', opts.solution, opts.dir, 'solution.c'),
          fixtureFile = util.codeWriteSync('c', opts.fixture, opts.dir, 'fixture.c'),
          args = ['clang-3.6', '-std=c11', fixtureFile, solutionFile, '-o', executable, './frameworks/c/criterion.c', '-I./frameworks/c', '-lcriterion', '-lm'];
      if (opts.setup) {
        var setupFile = util.codeWriteSync('c', opts.setup, opts.dir, 'setup.c');
        args.push(setupFile);
      }
      exec(args.join(' '), function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        opts.publish('stdout', stdout);
        runCode({'name': executable, 'args': ['-q','-j1']});
      });
    },
    sanitizeStdErr: function(error) {
      error = error || '';
      return error.replace(/clang.*-std=c[^\s]+/g, '')
                        .replace(/Error: Command failed:/g, '')
                        .replace(/\/tmp.*(solution\.c|solution)[:0-9]*/g, '')
                        .replace('\n', '')
                        .replace('  ', ' ')
                        .replace(opts.setup || '', '')
                        .replace(opts.fixture || '', '');
    }
  });
};

