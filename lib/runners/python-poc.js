const shovel = require('../shovel');
const util = require('../util');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    // python3 code.py
    // .
    // |-- code.py
    // `-- setup.py
    // file name can be configured by having a comment in first line
    solutionOnly(runCode) {
      if (opts.setup) {
        const m = opts.setup.match(/^#\s*name:\s*([a-z]+)/);
        util.writeFileSync(opts.dir, `${m === null ? 'setup' : m[1]}.py`, opts.setup, true);
      }
      runCode({
        name: 'python3',
        args: [util.writeFileSync(opts.dir, `code.py`, opts.code, true)]
      });
    },

    // python3 test
    // .
    // |-- setup.py
    // |-- solution.py
    // `-- test
    //     |-- __init__.py
    //     |-- __main__.py
    //     `-- test_solution.py
    // inspired by http://stackoverflow.com/a/27630375
    testIntegration(runCode) {
      if (opts.setup) {
        const m = opts.setup.match(/^#\s*name:\s*([a-z]+)/);
        util.writeFileSync(opts.dir, `${m === null ? 'setup' : m[1]}.py`, opts.setup, true);
      }
      util.writeFileSync(opts.dir, 'solution.py', opts.solution, true);
      util.writeFileSync(opts.dir+'/test', 'test_solution.py', opts.fixture, true);
      util.writeFileSync(opts.dir+'/test', '__init__.py', '', true);
      util.writeFileSync(opts.dir+'/test', '__main__.py', [
        'import unittest',
        'from codewars import CodewarsTestRunner',
        '',
        'def load_tests(loader, tests, pattern):',
        '    return loader.discover(".")',
        '',
        'unittest.main(testRunner=CodewarsTestRunner())',
        '',
      ].join('\n'), true);
      runCode({
        name: 'python3',
        args: ['test'],
        options: {
          cwd: opts.dir,
          env: Object.assign({}, process.env, {
            PYTHONPATH: `/runner/frameworks/python-poc:${process.env.PYTHONPATH}`
          })
        }
      });
    }
  });
};
