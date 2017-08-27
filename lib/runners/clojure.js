"use strict";

const uberJar = '/runner/jvm-runner/target/jvm-runner-0.1.3-standalone.jar';

module.exports = {
  solutionOnly(opts, runCode) {
    runCode({
      name: 'java',
      args: ['-jar', uberJar],
      stdin: JSON.stringify(opts)
    });
  },
  testIntegration(opts, runCode) {
    return runCode({
      name: 'java',
      args: ['-jar', uberJar],
      stdin: JSON.stringify(opts)
    });
  },
  // HACK: don't know clojure well enough to fix issue within actual runner, but it is escaping line breaks when it shouldn't
  sanitizeStdOut(opts, stdout) {
    return stdout
      .replace(/\<:LF:\>\<PASSED::\>/g, '\n<PASSED::>')
      .replace(/\<:LF:\>\<FAILED::\>/g, '\n<FAILED::>')
      .replace(/\<:LF:\>\<ERROR::\>/g, '\n<ERROR::>');
  }
};
