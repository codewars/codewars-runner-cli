var shovel = require('../shovel'),
    path = require('path');

module.exports.run = function run(opts, cb) {
  var uberJar = path.resolve(__dirname, '../../jvm-runner/target/jvm-runner-0.1.3-standalone.jar');
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      runCode({
        name: 'java',
        args: ['-jar', uberJar],
        stdin: JSON.stringify(opts)
      });
    },
    testIntegration: function(runCode) {
      return runCode({
        name: 'java',
        args: ['-jar', uberJar],
        stdin: JSON.stringify(opts)
      });
    },
    // HACK: don't know clojure well enough to fix issue within actual runner, but it is escaping line breaks when it shouldn't
    sanitizeStdOut: function(stdout) {
      return stdout
                .replace(/\<:LF:\>\<PASSED::\>/g, '\n<PASSED::>')
                .replace(/\<:LF:\>\<FAILED::\>/g, '\n<FAILED::>')
                .replace(/\<:LF:\>\<ERROR::\>/g, '\n<ERROR::>');

    }
  });
};
