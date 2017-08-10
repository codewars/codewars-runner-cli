"use strict";

const shovel = require('../shovel');
const path = require('path');
const fs = require('fs-extra');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly(runCode) {
      const dir = path.join(opts.dir, 'project');
      fs.outputFileSync(path.join(dir, 'build.gradle'), fs.readFileSync('/runner/frameworks/gradle/build.gradle'));
      if (opts.setup)
        fs.outputFileSync(path.join(dir, 'src', 'main', 'scala', 'setup.scala'), opts.setup);
      fs.outputFileSync(path.join(dir, 'src', 'main', 'scala', 'Main.scala'), opts.solution);

      runCode({
        name: 'gradle',
        args: [
          '--daemon',
          '--stacktrace',
          '--no-search-upward',
          '--offline',
          '--exclude-task', 'compileGroovy',
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

    testIntegration(runCode) {
      const dir = path.join(opts.dir, 'project');
      fs.outputFileSync(path.join(dir, 'build.gradle'), fs.readFileSync('/runner/frameworks/gradle/build.gradle'));
      if (opts.setup)
        fs.outputFileSync(path.join(dir, 'src', 'main', 'scala', 'Setup.scala'), opts.setup);
      fs.outputFileSync(path.join(dir, 'src', 'main', 'scala', 'Solution.scala'), opts.solution);
      fs.outputFileSync(path.join(dir, 'src', 'test', 'scala', 'Fixture.scala'), opts.fixture);
      runCode({
        name: 'gradle',
        args: [
          '--daemon',
          '--no-search-upward',
          '--offline',
          '--exclude-task', 'compileGroovy',
          '--exclude-task', 'compileKotlin',
          '--exclude-task', 'compileTestGroovy',
          '--exclude-task', 'compileTestKotlin',
          '--quiet',
          'test',
        ],
        options: {
          cwd: dir,
        }
      });
    },

    sanitizeStdErr(err) {
      const m = err.match(/\n\d+ tests? completed, \d+ failed\n\nFAILURE/);
      return m === null ? err : err.slice(0, m.index);
    },
  });
};


function getPrefix(code) {
  // TODO improve pattern and fail early if invalid
  const m = code.match(/^\s*package\s+(\S+)/m);
  return (m === null) ? '' : m[1] + '.';
}
