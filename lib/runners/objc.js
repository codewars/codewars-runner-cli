var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp'),
    path = require('path'),
    fs = require('fs'),
    exec = require('child_process').exec;

function compile(args, cb) {
    args.unshift('clang', '`gnustep-config --objc-flags --objc-libs`');
    args.push(' `gnustep-config  --base-libs`');
    exec(args.join(' '), cb);
}

function getCode(opts)
{
    const code = [];

    if (opts.mode === "default")
    {
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
    else
    {
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
}

function prepareCW(opts, args){
    var main = `
        #import <Foundation/Foundation.h>
        #import <CW/CWTest.h>

        ${opts.setup ? '#import "setup.m"' : ''}
        #import "solution.m"
        int main (int argc, const char * argv[]) {
            @autoreleasepool {
                ${opts.fixture}
            }
            return 0;
        }
    `;

    var setupFile = opts.setup ? util.codeWriteSync('objc', opts.setup, opts.dir, 'setup.m') : null,
        solutionFile = util.codeWriteSync('objc', opts.solution, opts.dir, 'solution.m'),
        mainFile = util.codeWriteSync('objc', main, opts.dir, 'main.m');

    return [ mainFile];    
}

function prepareUnitKit(opts){
    var fixtureHeader = `
        #import <Foundation/Foundation.h>
        #import <UnitKit/UnitKit.h>

        @interface TestSuite : NSObject <UKTest>
        @end
    `;    

    var fixture = `
        #import "solution.m"

        ${fixtureHeader}

        ${opts.fixture}
    `;

    var main = `
        ${fixtureHeader}

        #import <Foundation/Foundation.h>
        // our custom runner
        #import <UnitKit/CWRunner.h>

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

                [testReporter release];
                [testSuite release];
            }
            return status;
        }
    `;
        
    var solutionFile = util.codeWriteSync('objc', getCode(opts), opts.dir, 'solution.m'),
    fixtureFile = util.codeWriteSync('objc', fixture, opts.dir, 'fixture.m'),
    mainFile = util.codeWriteSync('objc', main, opts.dir, 'main.m');

    return [mainFile, fixtureFile, '-lUnitKit'];
}

module.exports.run = function run(opts, cb) {
    var args = [];

    shovel.start(opts, cb, {
        solutionOnly: function (runCode, fail) {
            var executable = path.join(opts.dir, 'solution');

            opts.solution = getCode(opts);

            var solutionFile = util.codeWriteSync('objc', opts.solution, opts.dir, 'solution.m');

            args = ['-o', executable, solutionFile];

            compile(args, function (error, stdout, stderr) {
                if (error) return fail(error, stdout, stderr);
                opts.publish('stdout', stdout);
                runCode({'name': executable, 'args': []});
            });
        },
        testIntegration: function (runCode, fail) {

            var executable = path.join(opts.dir, 'solution');
            args = ['-o', executable];

            switch (opts.testFramework)
            {
                case 'cw':
                    args = args.concat(prepareCW(opts, runCode));
                    break;
                case 'unitkit':
                    args = args.concat(prepareUnitKit(opts, runCode));
                    break;
                default:
                    throw `Test framework: ${opts.testFramework} not supported`
            }

            compile(args, function (error, stdout, stderr) {
                if (error) return fail(error, stdout, stderr);
                opts.publish('stdout', stdout + stderr);
                runCode({'name': executable, 'args': []});
            });
        },
        // objc NSLog is the stanard way of debugging, but everything goes to stderr. Fortunately normal
        // log messages also contain a timestamp prefix, so we can identify these messages and move them to stdout.
        // The one main issue here is that if anything is written to stdout, it won't be interleaved together.
        transformBuffer: function(buffer) {
            let stdout = buffer.stdout, stderr = buffer.stderr;
            buffer.stderr = '';
            stderr.split(/\n/gm).forEach(line => {
                let newLine = line.replace(/^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{3,4} \w*\[[\d:\w]*\]  ?/, '');
                //remove UnitKit output
                newLine = newLine.replace(/=== \[[\w \d]*\] ===/, '');
                //remove StackTrace
                newLine = newLine.replace(/Stack trace: \(.*\)/, '');
                if (line == newLine) {
                    buffer.stderr += line + "\n";
                }
                else {
                    if (newLine)
                        buffer.stdout += newLine + "\n";
                }
            });

            // if there is nothing but empty lines, clear the stderr
            if (buffer.stderr.replace(/[ \n]/g, '') == '') {
                buffer.stderr = '';
            }
        },
        sanitizeStdErr: function(error)
        {
            error = error || ''
            return error.replace(/clang.*gnustep-config.*--base-libs.\t*/g, '')
                        .replace(/Error: Command failed:/g, '')
                        .replace(/\/home.*(solution\.m|solution)[:0-9]*/g, '')
                        .replace(/\/home.*(fixture\.m|fixture)[:0-9]*/g, '')
                        .replace('\n', '')
                        .replace('  ', ' ')
                        .replace(opts.setup || '', '')
                        .replace(opts.fixture || '', '');
        }
    });
};
