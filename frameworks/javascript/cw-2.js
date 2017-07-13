
try {
  var util = require('util'),
      deepEquals = require('lodash').isEqual,
      Promise = require("bluebird");

  require('./chai-display');

  var fnToString = Function.toString;
  Function.prototype.toString = function() {
    switch (this) {
      case Test.expect:
      case Test.randomNumber:
      case Test.randomize:
      case Test.randomToken:
        return '[Codewars Code]';

      default:
        return fnToString.call(this);
    }
  };

  var describing = [],
      async = false,
      asyncIts = null,
      asyncDone = null,
      failed = [],
      beforeCallbacks = [],
      afterCallbacks = [],
      alwaysExplain = false;

  process.on('uncaughtException', function(err) {
    if (async) {
      Test.handleError(err);
      if (asyncDone) asyncDone();
    }
  });

  var describeNext = function() {
    if (asyncIts.length > 0) {
      asyncIts.shift()();
    }
    else if (describing.length) {
      describing.pop()();
    }
  };

  var _expect = function(passed, failMsg, options) {
    options = options || {};

    if (passed) {
      var successMsg = "Test Passed";
      if (options.successMsg) {
        successMsg += ": " + options.successMsg;
      }
      Test.display.write("PASSED", successMsg);

      if (options.passed) {
        options.passed(successMsg, options);
      }
    }
    else {
      failMsg = failMsg || 'Value is not what was expected';

      Test.display.write("FAILED", failMsg);

      if (options.failed) {
        options.failed(failMsg, options);
      }

      var error = new Test.Error(failMsg);
      if (describing) {
        failed.push(error);
      }
      else {
        throw error;
      }
    }
  };

  // convenience method for adding a default failed callback to an options
  var _failed = function(options, callback) {
    options = options || {};
    options.failed = options.failed || callback;
    return options;
  };

  var Test = {
    // when called, it will set a flag to cause all failed tests to explain the expected/actual results
    explainAll: function(mode) {
      alwaysExplain = mode || true;
    },

    display: require('./display'),

    // we use this instead of util.inspect so that we can support the indent option and json options
    stringify: function(obj, indent) {
      var cache = [];
      return JSON.stringify(obj, function(key, value) {
        if (typeof value === 'object' && value !== null) {
          // Circular reference found, discard key
          if (cache.indexOf(value) !== -1) return "[Circular]";
        }
        // Store value in our collection
        cache.push(value);
        return value;
      }, indent);
    },

    // backwards compatibility
    format: function(obj, options) {
      Test.display.format(obj, options);
    },
    inspect: function(obj) {
      // format arrays ourselves since long arrays end up getting broken out into separate lines, which is often a
      // bad format for typical use cases.
      if (Array.isArray(obj)) {
        return "[" + obj.map(function(v) {
          return Test.inspect(v);
        }).join(", ") + "]";
      }
      return util.inspect(obj);
    },
    log: function(msg, opts) {
      Test.display.log(msg, opts);
    },
    describe: function(msg, asyncTimeout, fn) {
      return new Promise(function(resolve, reject) {
        if (!fn) {
          fn = asyncTimeout;
          asyncTimeout = false;
        }
        else if (asyncTimeout === true) {
          asyncTimeout = 2000; // default timeout to 2 seconds
        }

        var start = new Date();
        try {
          async = asyncTimeout;
          asyncIts = [];
          describing.push(function() {
            var ms = new Date() - start;
            Test.display.write("COMPLETEDIN", ms);

            // TODO: right now before/after blocks don't work very well with multi level describes so they
            // should only be used at the top level
            if (!describing.length) {
              beforeCallbacks = [];
              afterCallbacks = [];
            }

            if (failed.length > 0) throw failed[0];

            async = false;
            resolve();
          });

          Test.display.write("DESCRIBE", msg);
          fn();
          if (async) describeNext();
        }
        catch (ex) {
          Test.handleError(ex);
        }
        finally {
          if (!async && describing.length) describing.pop()();
        }
      });

    },
    it: function(msg, fn) {
      if (!describing.length) throw '"it" calls must be invoked within a parent "describe" context';
      var asyncIt = (async && fn.length > 0);

      var begin = function() {
        Test.display.write("IT", msg);

        beforeCallbacks.forEach(function(cb) {
          cb();
        });

        var start = new Date(),
            timeout,
            done = function() {
              if (timeout) clearTimeout(timeout);

              var ms = new Date() - start;
              Test.display.write("COMPLETEDIN", ms);

              afterCallbacks.forEach(function(cb) {
                cb();
              });

              done = null;

              if (asyncIt) describeNext();
            };

        if (asyncIt) {
          timeout = setTimeout(function() {
            if (done) {
              throw "`it` function timed out. Function ran longer than " + async + "ms";
            }
          }, async);

          asyncDone = done;
        }

        try {
          fn(asyncIt ? done : undefined);
        }
        catch (ex) {
          Test.handleError(ex);
        }
        finally {
          if (!asyncIt) done();
        }
      };

      // if async then we queue everything up first
      if (async) {
        asyncIts.push(begin);
      }
      // otherwise just run it
      else {
        begin();
      }
    },
    before: function(cb) {
      beforeCallbacks.push(cb);
    },
    after: function(cb) {
      afterCallbacks.push(cb);
    },
    // handles an error and writes the appropriate output. If a function is provided it will handle the error
    // if the function errors, and then rethrow the exception
    handleError: function(ex) {
      if (typeof ex == "function") {
        try {
          ex();
        }
        catch (ex) {
          this.handleError(ex);
          throw ex;
        }
      }
      else if (ex.name == 'AssertionError') {
        this.fail(ex.message);
      }
      else if (ex.name != "TestError") {
        Test.display.write("ERROR", Test.trace(ex));
      }
    },
    // clean up the stack trace of the exception so that it doesn't give confusing results.
    // Results would be confusing because the user submitted code is compiled into a script where
    // additional code is injected and line numbers will not match.
    trace: function(ex) {
      return (ex.stack || ex.toString() || '')
        .toString()
        // remove file names (ie: (/cli-runner/...))
        .replace(/\s\(.*\)/g, '')
        // remove at [eval] statements
        .replace(/(at)*( Object.)*\s*[(]?\[eval\].*(:\d*)*[)]?\n/g, '')
        // remove stack trace beyond the Module information
        .replace(/at Module[\w\s.:\d\n]*/g, '')
        // remove at Object.<anonymous>
        .replace(/\t*at Object.<\w*>\n/g, '')
        // handleError is used to wrap top level code, so lets remove it so that it doesn't
        // confuse users who won't understand why it is there.
        .replace('at Object.Test.handleError', '');
    },
    pass: function() {
      _expect(true);
    },
    fail: function(message) {
      _expect(false, message);
    },
    expect: function(passed, message, options) {
      _expect(passed, message, options);
    },
    assertSimilar: function(actual, expected, msg, options) {
      this.assertEquals(Test.inspect(actual), Test.inspect(expected), msg, options);
    },
    assertNotSimilar: function(actual, expected, msg, options) {
      this.assertNotEquals(Test.inspect(actual), Test.inspect(expected), msg, options);
    },
    assertEquals: function(actual, expected, msg, options) {
      if (typeof(msg) == 'object') {
        options = msg;
        msg = null;
      }

      if (actual !== expected) {
        var explain = options && options.explain ? options.explain : alwaysExplain;

        if (explain) {
          Test.expect(false, msg || "Values should be equal", _failed(options, function() {
            Test.display.explain(actual, expected, {mode: explain, className: 'failed'});
          }));
        }
        else {
          Test.expect(false, Test.display.message('Expected: ' + Test.inspect(expected) + ', instead got: ' + Test.inspect(actual), msg));
        }
      }
      else {
        options = options || {};
        options.successMsg = options.successMsg || 'Value == ' + Test.inspect(expected);
        Test.expect(true, null, options);
      }
    },
    assertNotEquals: function(actual, expected, msg, options) {
      if (typeof(msg) == 'object') {
        options = msg;
        msg = null;
      }

      if (actual === expected) {
        var explain = options && options.explain ? options.explain : alwaysExplain;

        if (explain) {
          Test.expect(false, msg || "Values should not equal each other", _failed(options, function() {
            Test.display.explain(actual, expected, {mode: explain, className: 'failed'});
          }));
        }
        else {
          msg = Test.display.message('Values should not be equal: ' + Test.inspect(actual), msg);
          Test.expect(false, msg, options);
        }

      }
      else {
        options = options || {};
        options.successMsg = options.successMsg || 'Value != ' + Test.inspect(expected);
        Test.expect(true, null, options);
      }
    },
    assertDeepEquals: function(actual, expected, msg, options) {
      if (deepEquals(actual, expected)) {
        options = options || {};
        options.successMsg = options.successMsg || 'Value deep equals ' + Test.inspect(expected);
        Test.expect(true, null, options);
      }
      else {
        msg = Test.display.message('Expected: ' + Test.inspect(expected) + ', instead got: ' + Test.inspect(actual), msg);
        Test.expect(false, msg, options);
      }
    },
    assertNotDeepEquals: function(actual, expected, msg, options) {
      if (!deepEquals(actual, expected)) {
        options = options || {};
        options.successMsg = options.successMsg || 'Value not deep equals ' + Test.inspect(expected);
        Test.expect(true, null, options);
      }
      else {
        msg = Test.display.message('Value should not deep equal ' + Test.inspect(actual), msg);
        Test.expect(false, msg, options);
      }
    },
    assertApproxEquals: function(actual, expected, msg, options) {
      // Compares two floating point values and checks whether they are approximately equal to each other
      options = options || {};
      msg = Test.display.message('Expected actual value ' + actual + ' to approximately equal expected value ' + expected + ' (accepted relative error: 1e-9)', msg);
      if (Math.abs(expected) <= 1) {
        Test.expect(Math.abs(expected - actual) <= 1e-9, msg, options);
      }
      else {
        Test.expect(Math.abs((expected - actual) / expected) <= 1e-9, msg, options);
      }
    },
    assertNotApproxEquals: function(actual, unexpected, msg, options) {
      // Compares two floating point values and checks whether they are sufficiently different from each other
      options = options || {};
      msg = Test.display.message('Actual value ' + actual + ' should not approximately equal unexpected value ' + unexpected + ' (rejected relative error: 1e-9)', msg);
      if (Math.abs(unexpected) <= 1) {
        Test.expect(Math.abs(unexpected - actual) > 1e-9, msg, options);
      }
      else {
        Test.expect(Math.abs((unexpected - actual) / unexpected) > 1e-9, msg, options);
      }
    },
    assertContains: function(actual, expected, msg, options) {
      if (actual.indexOf(expected) >= 0) {
        options = options || {};
        options.successMsg = options.successMsg || 'Value ' + Test.inspect(actual) + ' contains ' + Test.inspect(expected);
        Test.expect(true, null, options);
      }
      else {
        msg = Test.display.message('Value ' + Test.inspect(actual) + ' should contain ' + Test.inspect(expected), msg);
        Test.expect(false, msg, options);
      }
    },
    assertNotContains: function(actual, expected, msg, options) {
      if (actual.indexOf(expected) === -1) {
        options = options || {};
        options.successMsg = options.successMsg || 'Value ' + Test.inspect(actual) + ' does not contain ' + Test.inspect(expected);
        Test.expect(true, null, options);
      }
      else {
        msg = Test.display.message('Value ' + Test.inspect(actual) + ' should not contain ' + Test.inspect(expected), msg);
        Test.expect(false, msg, options);
      }
    },
    expectNoError: function(msg, fn) {
      if (!fn) {
        fn = msg;
        msg = 'Unexpected error was thrown';
      }

      try {
        fn();
        Test.expect(true);
      }
      catch (ex) {
        if (ex.name == 'TestError') {
          throw ex;
        }
        else {
          msg += ': ' + ex.toString();
          Test.expect(false, msg);
        }
      }
    },
    expectError: function(msg, fn, options) {
      if (!fn) {
        fn = msg;
        msg = 'Expected an error to be thrown';
      }

      var passed = false;
      try {
        fn();
      }
      catch (ex) {
        console.log('<b>Expected error was thrown:</b> ' + ex.toString());
        passed = true;
      }

      Test.expect(passed, msg, options);
    },
    randomNumber: function() {
      return Math.floor(Math.random() * 101);
    },
    randomToken: function() {
      return Math.random().toString(36).substr(8);
    },
    randomize: function(array) {
      var arr = array.concat(), i = arr.length, j, x;
      while (i) {
        j = (Math.random() * i) | 0;
        x = arr[--i];
        arr[i] = arr[j];
        arr[j] = x;
      }
      return arr;
    },
    sample: function(array) {
      return array[~~(array.length * Math.random())];
    },
    escapeHtml: function(html) {
      return Test.display.escapeHtml(html);
    },
    Error: function(message) {
      this.name = "TestError";
      this.message = (message || "");
    }
  };

  //    Test.Error.prototype = require('assert').AssertionError.prototype;
  Test.Error.prototype = Error.prototype;

  Object.freeze(Test.display);
  Object.freeze(Test);

  Object.defineProperty(global, 'Test', {
    writable: false,
    configurable: false,
    value: Test
  });
  Object.defineProperty(global, 'describe', {
    writable: false,
    value: Test.describe
  });
  Object.defineProperty(global, 'it', {
    writable: false,
    value: Test.it
  });
  Object.defineProperty(global, 'before', {
    writable: false,
    value: Test.before
  });
  Object.defineProperty(global, 'after', {
    writable: false,
    value: Test.after
  });


}
catch (ex) {
  console.error(ex);
  throw "Failed to load core API methods";
}
