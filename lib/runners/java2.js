"use strict";

const shovel = require('../shovel');
const path = require('path');
const fs = require('fs-extra');

// TODO Remove SLF4J warnings
// SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder".
// SLF4J: Defaulting to no-operation (NOP) logger implementation
// SLF4J: See http://www.slf4j.org/codes.html#StaticLoggerBinder for further details.
module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly(runCode) {
      const dir = path.join(opts.dir, 'project');
      fs.outputFileSync(path.join(dir, 'build.gradle'), fs.readFileSync('/runner/frameworks/gradle/build.gradle'));
      if (opts.setup) {
        fs.outputFileSync(path.join(dir, 'src', 'main', 'java', `${getClassName(opts.setup)}.java`), opts.setup);
      }
      var solution = opts.solution;
      if (!/^\s*public\s+class\s+/m.test(solution)) {
        solution = solution.replace(/^\s*(class\s+\S+)/m, 'public $1');
      }
      const className = getClassName(solution);
      fs.outputFileSync(path.join(dir, 'src', 'main', 'java', `${className}.java`), solution);

      runCode({
        name: 'gradle',
        args: [
          '--daemon',
          '--stacktrace',
          '--no-search-upward',
          '--offline',
          '--project-cache-dir', '/runner/frameworks/gradle',
          '--exclude-task', 'compileGroovy',
          '--exclude-task', 'compileKotlin',
          '--exclude-task', 'compileScala',
          '--quiet',
          'run',
        ],
        options: {
          cwd: dir,
          env: Object.assign({}, process.env, {
            MAIN_CLASS_NAME: `${getPrefix(opts.solution)}${className}`,
          }),
        }
      });
    },

    testIntegration(runCode) {
      const dir = path.join(opts.dir, 'project');
      fs.outputFileSync(path.join(dir, 'build.gradle'), fs.readFileSync('/runner/frameworks/gradle/build.gradle'));
      if (opts.setup)
        fs.outputFileSync(path.join(dir, 'src', 'main', 'java', `${getClassName(opts.setup)}.java`), opts.setup);
      if (opts.solution)
        fs.outputFileSync(path.join(dir, 'src', 'main', 'java', `${getClassName(opts.solution)}.java`), opts.solution);
      if (opts.fixture)
        fs.outputFileSync(path.join(dir, 'src', 'test', 'java', `${getClassName(opts.fixture)}.java`), opts.fixture);
      if (opts.files) {
        for (const k of Object.keys(opts.files)) {
          const v = opts.files[k];
          fs.outputFileSync(path.join(dir, 'src', v.includes('@Test') ? 'test' : 'main', 'java', k), v);
        }
      }
      runCode({
        name: 'gradle',
        args: [
          '--daemon',
          '--no-search-upward',
          '--offline',
          '--project-cache-dir', '/runner/frameworks/gradle',
          '--exclude-task', 'compileGroovy',
          '--exclude-task', 'compileKotlin',
          '--exclude-task', 'compileScala',
          '--exclude-task', 'compileTestGroovy',
          '--exclude-task', 'compileTestKotlin',
          '--exclude-task', 'compileTestScala',
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

function getClassName(code) {
  const m = code.match(/^\s*(?:public\s+)?class\s+(\S+)/m);
  return (m === null) ? '' : m[1];
}
