var shovel = require('../shovel'),
    exec = require('child_process').exec,
    util = require('../util');


var groovyTestRunner = `
import org.junit.runner.JUnitCore;
import CwRunListener;
import TestFixture;

def runner =   new JUnitCore();
runner.addListener(new CwRunListener());
runner.run(TestFixture.class);
`;

module.exports.run = function run(opts, cb) {

  function compile(args, cb) {
    args.unshift('javac', '-verbose','-cp', opts.dir, '-d', opts.dir, '-sourcepath', opts.dir);
    exec(args.join(' '), cb);
  }

  shovel.start(opts, cb, {
    solutionOnly: function(runCode, fail) {
      var solutionFile = util.codeWriteSync('javac', opts.solution, opts.dir, 'Solution.java');

      compile([solutionFile], function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        opts.publish('stdout', stdout);
        runCode({
          name: 'groovyclient',
          args: ['-cp', opts.dir, '-e', 'import Solution; Solution.main(null);']
        });
      });
    },
    testIntegration: function(runCode, fail) {
      var solutionFile = util.codeWriteSync('javac', opts.solution, opts.dir, 'Solution.java');

      var fixtureFile = util.codeWriteSync('javac', opts.fixture, opts.dir, 'TestFixture.java');
      var runner = util.codeWriteSync('groovy', groovyTestRunner, opts.dir, 'testRunner.groovy');

      compile(['-cp', '/usr/local/groovy/lib/junit-4.12.jar', solutionFile, fixtureFile, './frameworks/java/CwRunListener.java'], function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        opts.publish('stdout', stdout);
        runCode({
          name: 'groovyclient',
          args: ['-cp', opts.dir, '-cp', '/usr/local/groovy/lib/', runner]
        });
      });
    }
  });
};
