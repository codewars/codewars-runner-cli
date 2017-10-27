"use strict";

const path = require('path');
const exec = require('child_process').exec;

const fs = require('fs-extra');

const frontendFrameworks = require('./javascript/frontend-frameworks');
const runKarma = require('./javascript/run-karma');

module.exports = {
  solutionOnly(opts, runCode, fail) {
    // TODO: Support Setup Code
    const solutionFile = path.join(opts.dir, 'solution.ts');
    fs.outputFileSync(solutionFile, opts.solution);
    exec(_tsc(opts) + ' ' + solutionFile, function(error, stdout, stderr) {
      if (error) return fail(error, stdout, stderr);
      runCode({name: 'node', args: [solutionFile.replace('.ts', '.js')]});
    });
  },
  testIntegration(opts, runCode, fail) {
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
        throw new Error('Test framework is not supported');
    }
  },
  sanitizeStdErr: sanitize,
  sanitizeStdOut: sanitize,
};

function sanitize(opts, s) {
  return s
    .replace(/(\()?\/codewars\/[(\w\/-\d.js:) ;]*/g, '')
    .replace(/( Object. )?[\(]?\[eval\][-:\w\d\)]* at/g, '')
    .replace(/Module._compile.*/g, '')
    .replace('Object.Test.handleError ', '')
    .replace('  ', ' ');
}

function prepareMocha(opts, interfaceType, runCode, fail) {
  const code = opts.setup ? `${opts.setup}\n${opts.solution}` : opts.solution;
  const codeFile = path.join(opts.dir, 'solution.ts');
  fs.outputFileSync(codeFile, code);
  const cmd = _tsc(opts);
  exec(cmd + ' ' + codeFile, function(error, stdout, stderr) {
    if (error) return fail(error, stdout, stderr);

    const specFile = path.join(opts.dir, 'spec.ts');
    fs.outputFileSync(specFile, opts.fixture);
    exec(cmd + ' ' + specFile, function(error, stdout, stderr) {
      if (error) return fail(error, stdout, stderr);
      const specFileJS = specFile.replace('.ts', '.js');
      runCode({
        name: 'mocha',
        args: [
          '-t', opts.timeout || 7000,
          '-u', interfaceType,
          '-R', '/runner/frameworks/javascript/mocha-reporter.js',
          specFileJS
        ]
      });
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
      const name = type === 'fixture' ? 'fixture.spec' : type;
      const file = path.join(dir, name + '.ts');
      fs.outputFileSync(file, opts[type]);
      files.push(file);
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
      },
      coverageOptions: {
        // A boolean indicating whether the code should be instrumented,
        // set this property to false to see the original Typescript code when debugging.
        instrumentation: false,
      },
    },
  };

  const nodeOpts = Object.assign({}, opts);
  // run on the default node version
  delete nodeOpts.languageVersion;

  runKarma(nodeOpts, runCode, fail, interfaceType, config);
}

function _tsc(opts) {
  const m = (opts.languageVersion || '2.4/ES3').match(/ES(?:[356]|201[5-7]|Next)$/);
  return [
    'tsc',
    '--module', 'commonjs',
    '--target', m === null ? 'ES3' : m[0],
  ].join(' ');
}
