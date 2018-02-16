"use strict";

const path = require('path');

const fs = require('fs-extra');

const writeFilesSync = require('../../utils/write-files-sync');
const frontendFrameworks = require('./frontend-frameworks');
const transform = require('./transform');
const maybeTransform = require('./maybe-transform');
const createNodeRunCommand = require('./create-node-run-command');
const execNode = require('./exec-node');
const runKarma = require('./run-karma');

module.exports = {
  solutionOnly(opts, runCode, fail) {
    execNode(opts, (opts.setup ? opts.setup + ';\n' : '') + opts.solution, runCode, fail);
  },
  testIntegration(opts, runCode, fail) {
    switch (opts.testFramework) {
      case 'cw': // for backwards compatibility, all legacy CW challenges have been updated to use cw-2
      case 'cw-2':
        return prepareCw2(opts, runCode, fail);

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
  files(opts) {
    // a files object/hash can be passed in as options, which will be transformed (if applicable) and then stored
    // in the provided directory.
    // write files that may have been included to the file system, within the directory that the code executes in
    writeFilesSync(opts.dir, opts.files, function(name, content) {
      return /\.js$/.test(name) ? maybeTransform(content, opts) : content;
    });
  }
};

function sanitize(opts, s) {
  return s
    .replace(/(\()?\/codewars\/[(\w\/-\d.js:) ;]*/g, '')
    .replace(/( Object. )?[\(]?\[eval\][-:\w\d\)]* at/g, '')
    .replace(/Module._compile.*/g, '')
    .replace('Object.Test.handleError ', '')
    .replace(opts.setup || '', '')
    .replace(opts.fixture || '', '');
}

function prepareMocha(opts, interfaceType, runCode) {
  if (!opts.solution && opts.files) {
    runMocha(opts, interfaceType, runCode, opts.entryPath);
  }
  else {
    var code = `
        ${opts.setup};
        ${opts.solution};
        (function() {
            ${opts.fixture};
        })();
    `;

    // run tests inline to the context but within their own scope. Redefine the key test methods in case they were
    // locally redefined within the solution.
    const solutionFile = path.join(opts.dir, 'mocha.js');
    fs.outputFileSync(solutionFile, maybeTransform(code, opts));
    runMocha(opts, interfaceType, runCode, solutionFile);
  }
}

function runMocha(opts, interfaceType, runCode, file) {
  runCode(createNodeRunCommand([
    '/.npm-global/bin/mocha',
    '-t', opts.timeout || 0,
    '-u', interfaceType,
    '-R', '/runner/frameworks/javascript/mocha-reporter.js',
    file
  ], opts));
}

function prepareKarma(opts, interfaceType, runCode, fail) {
  const dir = opts.dir;
  // files to load into Karma
  const files = [
    '/runner/node_modules/core-js/client/core.min.js',
  ];
  // handle includes
  if (opts.externalIncludes) {
    opts.externalIncludes.forEach(name => {
      files.push(...frontendFrameworks.find(name));
    });
  }

  // handle core code (if there is any)
  ['setup', 'solution', 'fixture'].forEach(function(type) {
    if (opts[type] && opts[type].length) {
      // always transform this code, it ends up in the browser:
      const code = transform(opts[type] || '', '0.10.x', type + '.js');
      const f = path.join(dir, type + '.js');
      fs.outputFileSync(f, code);
      files.push(f);
    }
  });

  // include any additional files
  if (opts.files) {
    Object.keys(opts.files).forEach(file => {
      // make sure to include the entry file last
      if (file !== opts.entryFile && file.match(/\.(js|css|html)$/)) {
        files.push(`${opts.dir}/${file}`);
      }
    });
  }

  // now include the entry file
  if (opts.entryPath) {
    files.push(opts.entryPath);
  }
  runKarma(opts, runCode, fail, interfaceType, {
    files: files,
  });
}

function prepareCw2(opts, runCode, fail) {
  var code;
  if (opts.projectMode) {
    code = `
            require('/runner/frameworks/javascript/cw-2');
            require('${opts.entryPath}');
        `;
  }
  else {
    // generate random identifier to avoid name collision
    const GLOBAL = `GLOBAL_${Math.random().toString(36).substr(2,8)}`;
    // run tests inline to the context but within their own scope. Redefine the key test methods in case they were
    // locally redefined within the solution.
    code = `
            require('/runner/frameworks/javascript/cw-2');
            var assert = require('assert');
            Test.handleError(function(){
                const ${GLOBAL} = global;
                ${opts.setup};
                ${opts.solution};
                (function() {
                    if (global != ${GLOBAL}) throw new Error('global was reassigned');
                    var Test = global.Test, describe = global.describe, it = global.it, before = global.before, after = global.after;
                    ${opts.fixture};
                })();
            });
        `;
  }

  execNode(opts, code, runCode, fail);
}
