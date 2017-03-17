const shovel = require('../shovel');
const writeFileSync = require('../util').writeFileSync;

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function (runCode) {
      runCode({
        name: 'nim',
        args: [
          'compile',
          '--run',            // run the compiled program
          '--define:release', // release build
          '--warnings:off',   // turn all warnings off
          '--hints:off',      // turn all hints off
          '--verbosity:0',    // set Nim's verbosity to minimal
          '--stackTrace:on',  // turn stack tracing on
          '--lineTrace:on',   // turn line tracing on
          '--checks:on',      // turn all runtime checks on
          writeFileSync(opts.dir, 'solution.nim', opts.solution, true)
        ],
        options: {
          cwd: opts.dir
        }
      });
    },

    testIntegration: function (runCode) {
      // TODO: look into module paths
      // test relies on /home/codewarrior/codewars/formatter.nim
      const dir = '/home/codewarrior';
      writeFileSync(dir, 'solution.nim', opts.solution, true);
      runCode({
        name: 'nim',
        args: [
          'compile',
          '--run',            // run the compiled program
          '--define:release', // release build
          '--warnings:off',   // turn all warnings off
          '--hints:off',      // turn all hints off
          '--verbosity:0',    // set Nim's verbosity to minimal
          '--stackTrace:on',  // turn stack tracing on
          '--lineTrace:on',   // turn line tracing on
          '--checks:on',      // turn all runtime checks on
          writeFileSync(dir, 'fixture.nim', opts.fixture, true)
        ],
        options: {
          cwd: dir
        }
      });
    },

    // Remove output from Nim
    // Error: execution of an external program failed: '/home/codewarrior/fixture '
    sanitizeStdErr: function(stderr) {
      const ss = stderr.split('\n');
      if (ss[0] == "Error: execution of an external program failed: '/home/codewarrior/fixture '") {
        return '';
      }
      return ss.join('\n');
    },
  });
};
