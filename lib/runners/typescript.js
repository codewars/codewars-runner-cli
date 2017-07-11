var shovel = require('../shovel'),
    util = require('../util'),
    exec = require('child_process').exec,
    jsRunner = require('./javascript');

module.exports.run = function run(opts, cb) {
  var dir = opts.dir;
  shovel.start(opts, cb, {
    solutionOnly: function(runCode, fail) {
      // TODO: Support Setup Code

      var solutionFile = util.codeWriteSync('typescript', opts.solution, dir, 'solution.ts', true);
      exec('tsc ' + solutionFile + ' --module commonjs', function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        runCode({name: 'node', args: [solutionFile.replace('.ts', '.js')]});
      });
    },
    testIntegration: function(runCode, fail) {
      switch (opts.testFramework) {
        case 'mocha':
        case 'mocha_bdd':
          return prepareMocha(opts, 'bdd', runCode, fail);
        case 'mocha_tdd':
          return prepareMocha(opts, 'tdd', runCode, fail);

        case 'karma':
        case 'karma_bdd':
          return prepareKarma(opts, 'bdd', runCode, fail);
        case 'karma_tdd':
          return prepareKarma(opts, 'tdd', runCode, fail);

        default:
          throw 'Test framework is not supported';
      }
    },
    sanitizeStdErr: function(error) {
      error = error || '';
      return error.replace(/(\()?\/codewars\/[(\w\/-\d.js:) ;]*/g, '')
                .replace(/( Object. )?[\(]?\[eval\][-:\w\d\)]* at/g, '')
                .replace(/Module._compile.*/g, '')
                .replace('Object.Test.handleError ', '')
                .replace('  ', ' ');
    },
    sanitizeStdOut: function(stdout) {
      return this.sanitizeStdErr(stdout);
    }
  });

  function prepareMocha(opts, interfaceType, runCode, fail) {
    var code = opts.setup ? `${opts.setup}\n${opts.solution}` : opts.solution;

    var codeFile = util.codeWriteSync('typescript', code, null, 'solution.ts', true);

    exec('tsc --module commonjs ' + codeFile, function(error, stdout, stderr) {
      if (error) return fail(error, stdout, stderr);

      var specFile = util.codeWriteSync('typescript', opts.fixture, null, 'spec.ts', true);

      exec('tsc --module commonjs ' + specFile, function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        specFile = specFile.replace('.ts', '.js');
        runCode({name: 'mocha', 'args': ['-t', opts.timeout || 7000, '-u', interfaceType, '-R', 'mocha-reporter', specFile]});
      });
    });
  }

  function prepareKarma(opts, interfaceType, runCode, fail) {
    var dir = opts.dir;
    // opts.dir = null;

    // files to load into Karma
    var files = [];

    // handle includes
    if (opts.externalIncludes) {
      opts.externalIncludes.forEach(name => {
        files.push(...frontendFrameworks.find(name));
      });
    }

    // handle core code (if there is any)
    ['setup', 'solution', 'fixture'].forEach(function(type) {
      if (opts[type] && opts[type].length) {
        var name = type === 'fixture' ? 'fixture.spec' : type;
        files.push(util.codeWriteSync('typescript', opts[type], dir, name + '.ts', true));
      }
    });

    // include any additional files
    if (opts.files) {
      Object.keys(opts.files).forEach(file => {
        // make sure to include the entry file last
        if (file !== opts.entryFile && file.match(/\.(ts|js|css|html)$/)) {
          files.push(`${opts.dir}/${file}`);
        }
      });
    }

    // now include the entry file
    if (opts.entryPath) {
      files.push(opts.entryPath);
    }

    var config = {
      files: files,
      frameworks: ['mocha', 'chai', 'karma-typescript'],
      preprocessors: {
        '**/*.ts': ['karma-typescript'],
        '/**/*.ts': ['karma-typescript'],
      },
      reporters: ['karma-typescript', 'coderunner'],
      karmaTypescriptConfig: {
        bundlerOptions: {
          entrypoints: /\.spec\.ts$/,
          transforms: [
            // the `@@` tells the karma runner to convert this string to raw JS
            // This is required because we're writing the final config out
            // to a string to be executed by the runner.
            `@@require('karma-typescript-es6-transform')()`,
            `@@require('karma-typescript-angular2-transform')`,
          ],
          resolve: {
            extensions: ['.ts', '.tsx', '.js', '.json'],
            directories: [
              '/runner/node_modules',
            ],
          }
        },
        compilerOptions: {
          module: 'commonjs',
          emitDecoratorMetadata: true,
          experimentalDecorators: true,
          lib: ['ES2015', 'DOM'],
          target: 'ES5',
          baseUrl: '.',
          paths: {
            '*': [
              '*',
              '/runner/node_modules/*',
            ],
          },
        }
      },
    };

    jsRunner.runKarma(opts, runCode, fail, interfaceType, config);
  }

};
