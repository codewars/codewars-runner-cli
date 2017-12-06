"use strict";

const writeFileSync = require('../utils/write-file-sync');

module.exports = {
  solutionOnly(opts, runCode) {
    runVersion(opts, [opts.setup, opts.solution].join("\n"), runCode);
  },
  testIntegration(opts, runCode) {
    if (isNoConcat(opts)) return python3unittest(opts, runCode);
    var code;
    switch (opts.testFramework) {
      case 'cw':
      case 'cw-2':
        code = opts.projectMode ? cw2Project(opts) : cw2Code(opts);
        break;
      case 'unittest':
        // TODO: support projectMode for unittest, which should require
        // improving unittest support so that a specific test case called Test doesn't need
        // to be used
        code = unittestCode(opts);
        break;
      default:
        throw new Error('test framework is not supported');
    }
    runVersion(opts, code, runCode);
  },
  sanitizeStdErr(opts, err) {
    if (isNoConcat(opts)) return err;
    // get rid of some of the noisy content. We remove line numbers since
    // they don't match up to what the user sees and will only confuse them.
    return err
      .replace(/>\n\n+</g, '>\n<')
      .replace(/File "(<string>)?", line \d*,/g, '')
      .replace(' (most recent call last)', '')
      .replace(' in in ', ' in ');
  }
};

function runVersion(opts, code, run) {
  run({name: pythonCmd(opts), 'args': ['-c', code]});
}

function pythonCmd(opts) {
  // backward compatibility
  const v = opts.language === 'python3' ? '3.x' : opts.languageVersion;
  switch (v) {
    case '3.6':
    case '3.6.x':
      return 'python3.6';

    case '3.4.x':
    case '3.4':
    case '3.x':
    case '3':
      return 'python3';

    case '2.x':
    case '2.7':
    case '2':
    case '':
    case null:
    case undefined:
      return 'python';

    default:
      throw new Error(`python version ${v} not supported`);
  }
}

function isPython3(opts) {
  return opts.language === 'python3' || opts.languageVersion && opts.languageVersion[0] == '3';
}

function cw2Code(opts) {
  return [
    defaultImports(opts),
    CW2_IMPORT,
    opts.setup,
    opts.solution,
    opts.fixture
  ].join('\n');
}

function cw2Project(opts) {
  return [
    defaultImports(opts),
    CW2_IMPORT,
    pythonExec(opts)
  ].join('\n');
}

function unittestCode(opts) {
  return [
    opts.setup,
    opts.solution,
    defaultImports(opts),
    UNITTEST_IMPORT,
    opts.fixture,
    RUN_UNITTEST,
  ].join('\n');
}

function pythonExec(opts, file) {
  file = file || opts.entryPath;
  return isPython3(opts) ?
    `exec(compile(open("${file}", "rb").read(), "${file}", 'exec'))` :
    `execfile('${file}')`;
}

function defaultImports(opts) {
  return `try: from imp import reload
except: pass
reload(__import__('sys')).path.append('./frameworks/python')
reload(__import__('sys')).path.append('${opts.dir}')
`;
}

const UNITTEST_IMPORT = "unittest = reload(__import__('unittest'))";
const CW2_IMPORT = "test = Test = reload(__import__('cw-2'))";

// TODO: handle different test class names
// bit of a hack, hard-coding the DESCRIBE/COMPLETEDIN here
// however, Python doesn't seem to want to call start/endTestRun, so this will have to do for now
const RUN_UNITTEST = `
print("<DESCRIBE::>Tests")
unittest.TestLoader().loadTestsFromTestCase(Test).run(reload(__import__('unittestwrapper')).CwTestResult())
print("<COMPLETEDIN::>")
`;

// Returns true if running tests without concatenation is possible.
function isNoConcat(opts) {
  return opts.testFramework === 'unittest' && isPython3(opts);
}

// .
// |-- setup.py
// |-- solution.py
// `-- test
//     |-- __init__.py
//     |-- __main__.py
//     `-- test_solution.py
// inspired by http://stackoverflow.com/a/27630375
//
// for backward compatibility:
// - prepend `import unittest` to fixture if missing
// - prepend `from solution import *` to fixture to simulate concatenation
// - prepend `from setup import *` to solution to simulate concatenation
function python3unittest(opts, runCode) {
  let solution = opts.solution;
  if (opts.setup) {
    writeFileSync(opts.dir, 'setup.py', opts.setup);
    if (!/^\s*import setup\s*$/m.test(solution) && !/^\s*from setup\s+/m.test(solution)) {
      solution = 'from setup import *\n' + solution;
    }
  }
  writeFileSync(opts.dir, 'solution.py', solution);
  let fixture = opts.fixture;
  if (!/^\s*import\s+unittest/m.test(fixture)) fixture = 'import unittest\n' + fixture;
  if (!/^\s*import solution\s*$/m.test(fixture) && !/^\s*from solution\s+/m.test(fixture)) {
    fixture = 'from solution import *\n' + fixture;
  }
  writeFileSync(opts.dir+'/test', 'test_solution.py', fixture);
  writeFileSync(opts.dir+'/test', '__init__.py', '');
  writeFileSync(opts.dir+'/test', '__main__.py', [
    'import unittest',
    'from codewars import CodewarsTestRunner',
    '',
    'def load_tests(loader, tests, pattern):',
    '    return loader.discover(".")',
    '',
    'unittest.main(testRunner=CodewarsTestRunner())',
    '',
  ].join('\n'));
  runCode({
    name: pythonCmd(opts),
    args: ['test'],
    options: {
      cwd: opts.dir,
      env: Object.assign({}, process.env, {
        PYTHONPATH: `/runner/frameworks/python:${process.env.PYTHONPATH}`
      }),
    }
  });
}
