"use strict";

const execNode = require('./exec-node');

module.exports = function runKarma(opts, runCode, fail, interfaceType, config) {
  // default to 20 seconds, since karma/phantom can be slow to start up
  opts.timeout = opts.timeout || 20000;

  const sConfig = JSON.stringify(Object.assign({
    singleRun: true,
    autoWatch: false,
    retryLimit: 0,
    frameworks: ['mocha', 'chai'],
    browsers: ['PhantomJS'],
    plugins: ['karma-*', '/runner/frameworks/javascript/karma-coderunner-reporter.js'],
    reporters: ['coderunner'],
    logLevel: 'warn',
    client: {
      mocha: {
        timeout: opts.timeout,
        ui: interfaceType,
      },
    },
  }, config), null, 4)
    // convert any strings that start with `@@` to literal JS
    .replace(/"@@([^"]+)"/g, (_, literal) => literal.replace(/\\(.)/g, '$1'));

  execNode(opts, [
    `var KarmaServer = require('karma').Server;`,
    `var karma = new KarmaServer(${sConfig}, function(code) {`,
    `    process.exit(code);`,
    `});`,
    `karma.start();`,
  ].join('\n'), runCode, fail);
};
