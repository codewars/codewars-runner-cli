var display = require('./display'),
    chai = require('chai');

// setup chai to render success messages, which it normally doesn't do when used with CW-2
chai.use(function(_chai, util) {
  chai.Assertion.prototype.assert = function(expr, msg, negateMsg, expected, _actual, showDiff) {
    var ok = util.test(this, arguments),
        result = ok ? 'passed' : 'failed';

    display.write(result, util.getMessage(this, arguments));

    if (showDiff) {
      display.explain(_actual, expected, {
        collapsed: true,
        className: result,
        arguments: this._arguments,
        context: this._context
      });
    }
  };

  chai.Assertion.addMethod('withArguments', function() {
    this._arguments = [].slice.apply(arguments);
  });

  chai.Assertion.addMethod('withContext', function() {
    this._context= [].slice.apply(arguments);
  });
});

