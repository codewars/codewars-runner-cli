var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp'),
    exec = require('child_process').exec,
    fs = require('fs'),
    chpldir = path.resolve(__dirname, '..', '..', 'frameworks', 'chapel'),
    cw2 = fs.readFileSync(path.resolve(chpldir, 'cw-2.chpl'));

module.exports.run = function run(opts, cb) {
  temp.track();
  const dir = temp.mkdirSync('chpl');
  shovel.start(opts, cb, {
    solutionOnly: function(runCode, fail) {
      if (opts.setup) handleSetup(opts, dir);
      const executable = path.join(dir, 'solution');
      const solutionFile = util.codeWriteSync('chpl', opts.solution, dir, 'solution.chpl');
      compile([solutionFile, '-o', executable], function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        opts.publish('stdout', stdout);
        runCode({name: executable, args: []});
      });
    },

    testIntegration: function(runCode, fail) {
      if (opts.setup) handleSetup(opts, dir);
      const executable = path.join(dir, 'solution');
      const solutionFile = util.codeWriteSync('chpl', [
        cw2,
        opts.solution,
        opts.fixture,
      ].join('\n'), dir, 'solution.chpl');
      compile([solutionFile, '-o', executable], function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        opts.publish('stdout', stdout);
        runCode({name: executable, args: []});
      });
    },

    sanitizeStdErr: function(error) {
      error = error || '';
      return error.replace(/\/tmp.*(solution\.chpl|solution)[:0-9]*/g, '')
                           .replace(/Error: Command failed:/g, '')
                           .replace(/error: uncaught error/g, '')
                           .replace('\n', '')
                           .replace('  ', ' ');
    }
  });
};


function compile(args, cb) {
  exec(`chpl ${args.join(' ')}`, cb);
}

function handleSetup(opts, dir) {
  opts.solution = opts.setup + '\n' + opts.solution;
}
