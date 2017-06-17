var shovel = require('../shovel'),
    exec = require('child_process').exec,
    util = require('../util');

module.exports.run = function run(opts, cb) {

  shovel.start(opts, cb, {
    solutionOnly: function(runCode, fail) {
      const names = extractNames(opts.solution, 'Solution');
      const solutionFile = util.codeWriteSync('javac', opts.solution, opts.dir, names.file);

      compile([solutionFile], function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        opts.publish('stdout', stdout);
        runCode({
          name: 'groovyclient',
          args: [
            '-cp', opts.dir,
            '-e', `import ${names.full}; ${names.full}.main(null);`,
            '-Cauthtoken', 'groovy'
          ]
        });
      });
    },
    testIntegration: function(runCode, fail) {

      const solutionNames = extractNames(opts.solution, 'Solution');
      const solutionFile = util.codeWriteSync('javac', opts.solution, opts.dir, solutionNames.file);

      const fixtureNames = extractNames(opts.fixture, 'TestFixture');
      const fixtureFile = util.codeWriteSync('javac', opts.fixture, opts.dir, fixtureNames.file);

      const args = [
        '-cp', '/usr/local/groovy/lib/junit-4.12.jar',
        solutionFile,
        fixtureFile,
        './frameworks/java/CwRunListener.java',
      ];

      if (opts.setup) {
        const setupNames = extractNames(opts.setup);
        args.push(util.codeWriteSync('javac', opts.setup, opts.dir, setupNames.file));
      }

      compile(args, function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        opts.publish('stdout', stdout);

        const runner = util.codeWriteSync('groovy', groovyTestRunner(fixtureNames.full), opts.dir, 'testRunner.groovy');

        runCode({
          name: 'groovyclient',
          args: [
            '-Cauthtoken', 'groovy',
            '-cp', opts.dir,
            '-cp', '/usr/local/groovy/lib/',
            runner
          ]
        });
      });
    },
    sanitizeStdErr: function(error) {
      // we need to strip out the authtoken error that may show up
      if (error.match(/^WARN: old authtoken/)) {
        error = error.substr(error.indexOf('1961 port') + 10);
      }
      return error;
    }
  });

  function compile(args, cb) {
    args.unshift('javac', '-verbose','-cp', opts.dir, '-d', opts.dir, '-sourcepath', opts.dir);
    exec(args.join(' '), cb);
  }

  function extractNames(code, defaultClassName) {
    const packageName = (code.match(/\bpackage\s+([A-Z|a-z](?:[a-z|A-Z|0-9|_]|\.[A-Z|a-z])*)\W/)||[])[1];
    const className = (code.match(/\bclass\s+([A-Z][a-z|A-Z|0-9|_]*)\W/)||[])[1] || defaultClassName;

    return {
      'package': packageName,
      'class': className,
      full: packageName ? `${packageName}.${className}` : className,
      file: `${className}.java`
    };
  }

  function groovyTestRunner(fixtureName) {
    return `import org.junit.runner.JUnitCore;
  import CwRunListener;
  import ${fixtureName};
  
  def runner = new JUnitCore();
  runner.addListener(new CwRunListener());
  runner.run(${fixtureName}.class);
  `;
  }
};

