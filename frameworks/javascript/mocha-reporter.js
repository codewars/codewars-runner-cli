var Base = require('mocha/lib/reporters/base');

module.exports = Reporter;

function Reporter(runner) {
  Base.call(this, runner);

  runner.on('suite', function(suite) {
    if (suite.title) {
      console.log("\n<DESCRIBE::>" + format(suite.title));
    }
  });

  runner.on('suite end', function(suite) {
    if (suite.title) {
      console.log("\n<COMPLETEDIN::>");
    }
  });

  runner.on('test', function(test) {
    console.log('\n<IT::>' + format(test.title));
  });

  runner.on('test end', function(test) {
    console.log("\n<COMPLETEDIN::>");
  });

  runner.on('pass', function(test) {
    console.log('\n<PASSED::>Passed');
  });

  runner.on('fail', function(test, err) {
    if (err instanceof Error && test.err.name != 'AssertionError') {
      console.log('\n<ERROR::>' + format(err.stack || err.toString()));
    }
    else if (test.timedOut) {
      console.log('\n<FAILED::>' + 'Timed out');
    }
    else {
      console.log('\n<FAILED::>' + format(err.message));
    }
  });

  runner.on('end', function() {
    process.exit(0);
  });

  function format(text) {
    return text.replace(/\n/g, '<:LF:>');
  }
}
