"use strict";

const fs = require('fs');
const path = require('path');

const temp = require('temp').track();

const dir = '/tmp/dart';

module.exports = {
  solutionOnly(opts, runCode) {
    _createSolution(opts).then(file => {
      // Run
      runCode({
        name: 'dart',
        args: [file],
        options: {
          cwd: dir
        }
      });
    });
  },
  testIntegration(opts, runCode) {
    // test is the only one supported right now...
    if (opts.testFramework != 'test')
      throw new Error('Test framework is not supported');

    // Create the spec test
    _createSpec(opts).then(file => {
      // Run
      runCode({
        name: 'pub',
        args: ['run', 'test', file],
        options: {
          cwd: dir
        }
      });
    });
  },
  sanitizeStdOut(opts, stdout) {
    if (opts.fixture) return _format(stdout);
    return stdout;
  }
};

// Create the solution file
var _createSolution = function(opts) {
  // Set the code, the random var will stop user adding imports
  // that are not provided in setup
  let code = `
    ${opts.setup ? opts.setup : ''}

    ${opts.code}
  `;

  return _writeCode(code);
};

// Create the spec
var _createSpec = function(opts) {
  // Includes test suites
  let code = `
    import 'package:test/test.dart';
    import 'package:matcher/matcher.dart';
    ${opts.setup ? opts.setup : ''}

    ${opts.code}

    main() {
      ${opts.fixture}
    }

  `;

  return _writeCode(code);
};

// Helper to allow creating tracked temp file in specific dir
// which has the pubspec.yaml
var _writeCode = function(code) {
  let affix = {
    dir: dir,
    suffix: '.dart'
  };

  return new Promise((resolve, reject) => {
    temp.open(affix, (err, info) => {
      if (err) reject(err);
      else {
        // Write code & close
        fs.writeSync(info.fd, code);
        fs.closeSync(info.fd);

        // Also write this test config
        fs.writeFileSync(path.join(dir, 'dart_test.yaml'), 'reporter: json');

        resolve(info.path);
      }
    });
  });
};

// Formats the test output json
var _format = function(stdout) {
  try {

    let results = _coerceObjects(_parseLines(stdout));

    stdout = '';
    for (let id in results) {
      let test = results[id];
      stdout += `<IT::>${test.name}\n`;
      stdout += test.result == 'success' ? `<PASSED::>Test Passed\n` : `<FAILED::>Test Failed\n`;
      stdout += `<COMPLETEDIN::>${test.runTime}\n`;
    }
  }
  catch (err) {
    // Split up the error string to discard details about the dart file
    let msgs = err.split(/.*.dart["']:\s*(?:warning: |error: )?/).filter(Boolean);

    stdout = `<ERROR::>${msgs[0]}`;
  }
  return stdout;
};

// Parse stdout json lines
var _parseLines = function(stdout) {
  return stdout.split('\n').filter(v => v).map(JSON.parse);
};

// Convert objects to lovely test result objects
var _coerceObjects = function(lines) {
  let tests = {};

  lines.forEach(line => {
    if (!line.test && !line.testID) return;

    let test = {};

    // Retrieve or create
    if (line.test) {
      test.id = line.test.id;
      test.name = line.test.name;
      test.start = line.time;

      // Add entry to array
      tests[test.id] = test;
    }
    else if (line.hidden) {
      // Remove this test by id
      delete tests[line.testID];
    }
    else {
      if (line.error && line.isFailure === false) throw line.error;

      // Set the test values
      test = tests[line.testID];
      test.result = line.result;
      test.end = line.time;
      test.runTime = test.end - test.start;
    }
  });
  return tests;
};
