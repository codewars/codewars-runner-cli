"use strict";

const path = require('path');
const exec = require('child_process').exec;

const codeWriteSync = require('../utils/code-write-sync');

module.exports = {
  solutionOnly(opts, runCode, fail) {
    const executable = path.join(opts.dir, 'solution');
    opts.solution = getCode(opts);

    const solutionFile = codeWriteSync('objc', opts.solution, opts.dir, 'solution.m');
    const args = ['-o', executable, solutionFile];
    compile(args, opts.languageVersion, function(error, stdout, stderr) {
      if (error) return fail(error, stdout, stderr);
      opts.publish('stdout', stdout);
      runCode({'name': executable, 'args': []});
    });
  },
  testIntegration(opts, runCode, fail) {
    const executable = path.join(opts.dir, 'solution');
    var args = ['-o', executable];
    switch (opts.testFramework) {
      case 'cw':
        args = args.concat(prepareCW(opts));
        break;
      case 'unitkit':
        args = args.concat(prepareUnitKit(opts));
        break;
      default:
        throw new Error(`Test framework: ${opts.testFramework} not supported`);
    }

    compile(args, opts.languageVersion, function(error, stdout, stderr) {
      if (error) return fail(error, stdout, stderr);
      opts.publish('stdout', stdout + stderr);
      runCode({'name': executable, 'args': []});
    });
  },
  // objc NSLog is the stanard way of debugging, but everything goes to stderr. Fortunately normal
  // log messages also contain a timestamp prefix, so we can identify these messages and move them to stdout.
  // The one main issue here is that if anything is written to stdout, it won't be interleaved together.
  transformBuffer(opts, buffer) {
    let stderr = buffer.stderr;
    buffer.stderr = '';
    stderr.split(/\n/gm).forEach(line => {
      let newLine = line.replace(/^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{3,4} \w*\[[\d:\w]*\]  ?/, ''); // eslint-disable-line no-regex-spaces
      //remove UnitKit output
      newLine = newLine.replace(/=== \[[\w \d]*\] ===/, '');
      //remove StackTrace
      newLine = newLine.replace(/Stack trace: \(.*\)/, '');
      if (line == newLine) {
        buffer.stderr += line + "\n";
      }
      else if (newLine)
        buffer.stdout += newLine + "\n";
    });

    // if there is nothing but empty lines, clear the stderr
    if (buffer.stderr.replace(/[ \n]/g, '') == '') {
      buffer.stderr = '';
    }
  },
  sanitizeStdErr(opts, error) {
    return error.replace(/clang.*gnustep-config.*--base-libs.\t*/g, '').replace(/Error: Command failed:/g, '').replace(/\/home.*(solution\.m|solution)[:0-9]*/g, '').replace(/\/home.*(fixture\.m|fixture)[:0-9]*/g, '').replace('\n', '').replace('  ', ' ').replace(opts.setup || '', '').replace(opts.fixture || '', '');
  }
};


function getCode(opts) {
  if (opts.mode === "default") {
    return `
      #import <Foundation/Foundation.h>
      int main (int argc, const char *argv[]) {
          @autoreleasepool{
              ${opts.solution}
          }
          return 0;
      }
    `;
  }

  return `
      /* headers */
      // setup
      ${opts.setupHeader || ''}
      // solution
      ${opts.codeHeader || ''}
      /* code */
      // setup
      ${opts.setup || ''}
      // solution
      ${opts.solution || opts.code || ''}
    `;
}

function prepareCW(opts) {
  var main = `
      #import <Foundation/Foundation.h>
      #import <CW/CWTest.h>


      #ifndef __has_feature
        #define __has_feature(x) 0  // Compatibility with non-clang compilers.
      #endif
      #ifndef __has_extension
        #define __has_extension __has_feature // Compatibility with pre-3.0 compilers.
      #endif

      ${opts.setup ? '#import "setup.m"' : ''}
      #import "solution.m"
      int main (int argc, const char * argv[]) {
          @autoreleasepool {
              ${opts.fixture}
          }
          return 0;
      }
  `;

  codeWriteSync('objc', opts.solution, opts.dir, 'solution.m');
  if (opts.setup) {
    codeWriteSync('objc', opts.setup, opts.dir, 'setup.m');
  }
  return [codeWriteSync('objc', main, opts.dir, 'main.m')];
}

function prepareUnitKit(opts) {
  var fixtureHeader = `
      #import <Foundation/Foundation.h>
      #import <UnitKit/UnitKit.h>

      @interface TestSuite : NSObject <UKTest>
      @end
  `;

  var fixture = `
      ${opts.setup ? '#import "setup.m"' : ''}
      #import "solution.m"
      ${fixtureHeader}
      ${opts.fixture}
  `;

  var main = `
      ${fixtureHeader}

      #import <Foundation/Foundation.h>
      // our custom runner
      #import <UnitKit/CWRunner.h>

      #ifndef __has_feature
        #define __has_feature(x) 0  // Compatibility with non-clang compilers.
      #endif
      #ifndef __has_extension
        #define __has_extension __has_feature // Compatibility with pre-3.0 compilers.
      #endif

      int main (int argc, const char *argv[])
      {
          int status = EXIT_FAILURE;

          @autoreleasepool
          {

              TestSuite *testSuite = [TestSuite new];
              CWRunner* testReporter = [CWRunner new];

              [testReporter runSuite: [testSuite class] ];

              //int testsPassed = [testReporter testsPassed];
              int testsFailed = [testReporter testsFailed];
              int exceptionsReported = [testReporter exceptionsReported];

              //printf("\\nResult: %i tests, %i failed, %i exceptions\\n", (testsPassed + testsFailed), testsFailed, exceptionsReported);*/

              status = (testsFailed == 0 && exceptionsReported == 0 ? 0 : -1);


              #if !__has_feature(objc_arc)
                  // Manual memory management
                  [testReporter release];
                  [testSuite release];
              #else
                  // ARC enabled, do nothing...
              #endif
          }
          return status;
      }
  `;

  codeWriteSync('objc', opts.solution, opts.dir, 'solution.m');
  if (opts.setup) codeWriteSync('objc', opts.setup, opts.dir, 'setup.m');
  const fixtureFile = codeWriteSync('objc', fixture, opts.dir, 'fixture.m');
  const mainFile = codeWriteSync('objc', main, opts.dir, 'main.m');
  return [mainFile, fixtureFile, '-lUnitKit'];
}

function compile(args, version, cb) {
  switch (version) {
    case 'objc-arc':
      args.unshift('clang', '-fobjc-arc', '`gnustep-config --objc-flags --objc-libs`');
      break;

    case 'noobjc-arc':
    default:
      args.unshift('clang', '`gnustep-config --objc-flags --objc-libs`');
      break;
  }

  args.push(' `gnustep-config  --base-libs`');
  exec(args.join(' '), cb);
}
