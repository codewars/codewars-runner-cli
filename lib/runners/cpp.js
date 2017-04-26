var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp'),
    exec = require('child_process').exec,
    fs = require('fs'),
    cppDir = path.resolve(__dirname, '..', '..', 'frameworks', 'cpp'),
    main = fs.readFileSync(path.resolve(cppDir, 'main.cpp'));

module.exports.run = function run(opts, cb) {
  temp.track();
  const dir = temp.mkdirSync('cpp');
  shovel.start(opts, cb, {
    solutionOnly: function(runCode, fail) {
      if (opts.setup) handleSetup(opts, dir);
      const executable = path.join(dir, 'solution');
      const solutionFile = util.codeWriteSync('cpp', opts.solution, dir, 'solution.cpp');
      compile([solutionFile, '-o', executable], function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        opts.publish('stdout', stdout);
        runCode({name: executable, args: []});
      });
    },

    testIntegration: function(runCode, fail) {
      if (opts.setup) handleSetup(opts, dir);
      const executable = path.join(dir, 'solution');
      const solutionFile = util.codeWriteSync('cpp', [
        '#include <igloo/igloo_alt.h>',
        'using namespace igloo;',
        opts.solution,
        opts.fixture,
        main
      ].join('\n'), dir, 'solution.cpp');
      compile(['-isystem', cppDir, solutionFile, '-o', executable], function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        opts.publish('stdout', stdout);
        runCode({name: executable, args: []});
      });
    },

    sanitizeStdErr: function(error) {
      error = error || '';
      return error.replace(/clang.*-std=c[^\s]+/g, '')
                        .replace(/Error: Command failed:/g, '')
                        .replace(/\/tmp.*(solution\.cpp|solution)[:0-9]*/g, '')
                        .replace('\n', '')
                        .replace('  ', ' ')
                        .replace(opts.setup || '', '')
                        .replace(opts.fixture || '', '');
    }
  });
};


function compile(args, cb) {
  exec(`clang++-3.6 -stdlib=libc++ -std=c++1y ${args.join(' ')}`, cb);
}

function handleSetup(opts, dir) {
  util.codeWriteSync('cpp', opts.setup, dir, 'setup.h');
  opts.solution = '#include "setup.h"\n' + opts.solution;
}

