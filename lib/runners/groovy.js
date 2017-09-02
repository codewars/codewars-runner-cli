"use strict";

const path = require('path');

const fs = require('fs-extra');
const whenPrewarmed = require('../utils/when-prewarmed');

module.exports = {
  solutionOnly(opts, runCode) {
    const dir = path.join(opts.dir, 'project');
    fs.outputFileSync(path.join(dir, 'build.gradle'), fs.readFileSync('/runner/frameworks/gradle/build.gradle'));
    if (opts.setup)
      fs.outputFileSync(path.join(dir, 'src', 'main', 'groovy', 'setup.groovy'), opts.setup);
    fs.outputFileSync(path.join(dir, 'src', 'main', 'groovy', 'Main.groovy'), opts.solution);

    runCode({
      name: 'gradle',
      args: [
        '--daemon',
        '--stacktrace',
        '--no-search-upward',
        '--offline',
        '--exclude-task', 'compileScala',
        '--exclude-task', 'compileKotlin',
        '--quiet',
        'run',
      ],
      options: {
        cwd: dir,
        env: Object.assign({}, process.env, {
          MAIN_CLASS_NAME: `${getPrefix(opts.solution)}Main`,
        }),
      }
    });
  },

  testIntegration(opts, runCode) {
    const dir = path.join(opts.dir, 'project');
    fs.outputFileSync(path.join(dir, 'build.gradle'), fs.readFileSync('/runner/frameworks/gradle/build.gradle'));
    if (opts.setup)
      fs.outputFileSync(path.join(dir, 'src', 'main', 'groovy', 'Setup.groovy'), opts.setup);
    fs.outputFileSync(path.join(dir, 'src', 'main', 'groovy', 'Solution.groovy'), opts.solution);
    fs.outputFileSync(path.join(dir, 'src', 'test', 'groovy', 'Fixture.groovy'), opts.fixture);

    whenPrewarmed(opts, prewarmed => {
      runCode({
        name: 'gradle',
        args: [
          prewarmed ? '--daemon' : '--no-daemon', // if not prewarmed don't bother
          '--no-search-upward',
          '--offline',
          '--exclude-task', 'compileScala',
          '--exclude-task', 'compileKotlin',
          '--exclude-task', 'compileTestScala',
          '--exclude-task', 'compileTestKotlin',
          '--quiet',
          'test',
        ],
        options: {
          cwd: dir,
        }
      });
    });
  },

  sanitizeStdErr(opts, err) {
    const m = err.match(/\n\d+ tests? completed, \d+ failed\n\nFAILURE/);
    return m === null ? err : err.slice(0, m.index);
  },
};


function getPrefix(code) {
  // TODO improve pattern and fail early if invalid
  const m = code.match(/^\s*package\s+(\S+)/m);
  return (m === null) ? '' : m[1] + '.';
}
