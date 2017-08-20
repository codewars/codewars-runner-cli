"use strict";

const path = require('path');

const fs = require('fs-extra');

module.exports = {
  solutionOnly(opts, runCode) {
    runCode({
      name: 'php',
      args: [
        '-r',
        (opts.setup ? opts.setup + ';\n' : '') + opts.solution,
      ]
    });
  },
  testIntegration(opts, runCode) {
    switch (opts.testFramework) {
      case 'cw-2':
        return prepareCw2(opts, runCode);
      case 'phpunit':
        return preparePHPUnit(opts, runCode);
      default:
        throw new Error('Test framework is not supported');
    }
  },
  sanitizeStdOut(opts, output) {
    var lines = output.split('\n');
    return filterLines(lines).join('\n');
  },
  sanitizeStdErr(opts, error) {
    error = error ? `\n${error}` : '';
    return error.replace(/(Uncaught Exception: Failed Test).*/g, '$1').replace(/\/runner\/.*\.php/g, 'input');
  }
};

function prepareCw2(opts, run) {
  var code = `
        require_once('frameworks/php/cw-2.php');
        ${opts.setup || ''}
        ${opts.solution}
        $test = new Test;
        ${opts.fixture}
    `;

  run({name: 'php', 'args': ['-r', code]});
}

function preparePHPUnit(opts, run) {
  var code = `
        <?php
            use phpunit\\framework\\TestCase;
            ${opts.setup || ''}
            ${opts.solution}
            ${opts.fixture}
        ?>
    `;

  const file = path.join(opts.dir, 'run.php');
  fs.outputFileSync(file, code);
  run({name: 'phpunit', 'args': ['--configuration=frameworks/php/phpunit/phpunit.xml', file]});
}

var filterLines = (function() {
  var blacklist = [
    /^\s*$/,
    /^ *PHPUnit 5\.\d\.\d* by Sebastian Bergmann and contributors\./,
    /\s+\d \/ \d \(\d+\%\)/,
    /^Time: \d+ ms, Memory:/,
    /^There was \d+ failure/,
    /^FAILURES!$/,
    /^Tests: \d+, Assertions:/,
  ];

  /**
   * Strip lines from output we don't want
   *
   * @param array lines
   * @return array
   */
  return function(lines) {
    return lines.filter(function(line) {
      return blacklist.every(function(regex) {
        return !regex.test(line);
      });
    }).map(function(line) {
      // clean up filenames
      if (/\/tmp.*\.php/.test(line)) {
        line = line.replace(/\/tmp\/[^\/]+\//, '');
      }
      return line;
    });
  };
}());
