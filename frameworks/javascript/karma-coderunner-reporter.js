'use strict';
var util = require('util');

function print(type, text) {
  var msg = text ? ('' + text).replace(/\n/g, '<:LF:>') : '';
  console.log('<' + type + '::>' + msg);
}

function CodeRunnerReporter(baseReporterDecorator, formatError, config, helper) {
  // extend the base reporter
  baseReporterDecorator(this);

  var suites = [];

  this.onRunStart = function() {
    // nothing to do
  };

  this.onBrowserStart = function(browser) {
    // We could include the browser as an outer suite, but we're only running on PhantomJS, so there's no real benefit
    // print('DESCRIBE', browser.toString());
  };

  this.onBrowserError = function(browser, error) {
    print('ERROR', 'Error:\n' + formatError(error));
  };

  this.onBrowserLog = function(browser, log, type) {
    let msg = helper.isString(log) ? log.replace(/^'|'$/g, '') : util.inspect(log);
    // if(typeof console[type] !== 'function') type = 'log';
    console[type](msg);
  };

  this.onBrowserComplete = function(browser) {
    // We could include the browser as an outer suite, but we're only running on PhantomJS, so there's no real benefit
    // print('COMPLETEDIN');
  };

  this.onSpecComplete = function(browser, result) {
    if (!result.skipped) {
      _renderSuites((result.suite || []).slice());
      print('IT', result.description);
      if (result.success) {
        print('PASSED', result.time + 'ms');
      }
      else {
        print('FAILED', result.log);
      }
      print('COMPLETEDIN');
    }
  };

  this.onRunComplete = function(browsers, results) {
    _renderSuites([]);
  };

  function _renderSuites(specSuites) {
    var currentSuites = suites.slice();
    var finalSuites = [];
    var i;
    while (specSuites.length && specSuites[0] === currentSuites[0]) {
      finalSuites.push(specSuites.shift());
      currentSuites.shift();
    }
    for (i = 0; i < currentSuites.length; i++) {
      print('COMPLETEDIN');
    }
    for (i = 0; i < specSuites.length; i++) {
      print('DESCRIBE', specSuites[i]);
    }
    suites = finalSuites.concat(specSuites);
  }
}

CodeRunnerReporter.$inject = ['baseReporterDecorator', 'formatError', 'config', 'helper'];

module.exports = {
  'reporter:coderunner': ['type', CodeRunnerReporter]
};
