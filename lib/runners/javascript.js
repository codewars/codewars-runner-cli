var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config'),
    temp = require('temp'),
    child_process = require('child_process');

module.exports.run = function run(opts, cb)
{
    shovel.start(opts, cb, {
        solutionOnly: function (run)
        {
            var code = opts.solution;

            if (opts.setup)
                code = opts.setup + ';\n' + code;

            runNode(opts, code, run);
        },
        testIntegration: function (run)
        {
            switch (opts.testFramework)
            {
                case 'cw': // for backwards compatibility, all legacy CW challenges have been updated to use cw-2
                case 'cw-2':
                    return prepareCw2(opts, run);

                case 'mocha':
                case 'mocha_bdd':
                    return prepareMocha(opts, 'bdd', run);

                case 'mocha_tdd':
                    return prepareMocha(opts, 'tdd', run);

                default:
                    throw 'Test framework is not supported'
            }
        },
        sanitizeStdErr: function(error)
        {
            error = error || ''
            return error.replace(/(\()?\/codewars\/[(\w\/-\d.js:) ;]*/g, '')
                        .replace(/( Object. )?[\(]?\[eval\][-:\w\d\)]* at/g, '')
                        .replace(/Module._compile.*/g, '')
                        .replace('Object.Test.handleError ', '')
                        .replace('  ', ' ');
        },
        sanitizeStdOut: function(stdout)
        {
            return this.sanitizeStdErr(stdout);
        }
    });
};

function prepareMocha(opts, interfaceType, run) {
    var dir = temp.mkdirSync('javascript');
    var code = '';

    if (opts.setup)
    {
        code = opts.setup + ';\n';
    }

    code += opts.solution + ';\n';

    // run tests inline to the context but within their own scope. Redefine the key test methods in case they were
    // locally redefined within the solution.

    // TODO: instead of using one file, we should require the solution file into the spec file (see TypeScript implementation)
    code += config.snippets.javascript.closure.start;
    code += opts.fixture;
    code += config.snippets.javascript.closure.end;

    var solutionFile = util.codeWriteSync('javascript', transform(code), dir, 'mocha', true);
    // NOTE: Mocha based testing currently does not support Node versioning
    run({name: 'mocha', 'args': ['--harmony', '-u', interfaceType, '-R', 'mocha-reporter', solutionFile]});
}

function prepareCw2(opts, run)
{
    var code = config.snippets.javascript.requireCw2;
    code += "var assert = require('assert');\n";

    code += config.snippets.javascript.start;

    if (opts.setup)
    {
        code += opts.setup + ';\n';
    }

    code += opts.solution + ';\n';

    // run tests inline to the context but within their own scope. Redefine the key test methods in case they were
    // locally redefined within the solution.
    code += ';' + config.snippets.javascript.inlineTestFixture.start;
    code += opts.fixture;
    code += config.snippets.javascript.inlineTestFixture.end;
    code += config.snippets.javascript.end;

    runNode(opts, code, run);
}

function runNode(opts, code, run) {
    var version = (opts.languageVersion || '6.x').split('/')[0]

    // support 6.x/babel
    if (opts.languageVersion && opts.languageVersion.split('/')[1] == 'babel') {
        code = transform(code);
    }
    run({name: `/usr/local/n/versions/node/${nodeVersion(version)}/bin/node`, 'args': ['-e', code]});
}

function nodeVersion(version) {
    switch(version) {
        case "6.x":
            return "6.0.0";
        case "0.10.x":
            return "0.10.33"
        default:
            return version;
    }
}

function transform(code, version) {
    try {
        if (version == '0.10.x') {
            return require('babel-core').transform(code, {
                presets: ["stage-1", "react"],
                plugins: [
                    "transform-runtime",
                    "check-es2015-constants",
                    "angular2-annotations",
                    "transform-decorators-legacy",
                    "transform-class-properties",
                    "transform-flow-strip-types",
                    "transform-es2015-arrow-functions",
                    "transform-es2015-block-scoped-functions",
                    "transform-es2015-block-scoping",
                    "transform-es2015-classes",
                    "transform-es2015-computed-properties",
                    "transform-es2015-destructuring",
                    "transform-es2015-duplicate-keys",
                    "transform-es2015-for-of",
                    "transform-es2015-function-name",
                    "transform-es2015-literals",
                    "transform-es2015-object-super",
                    "transform-es2015-parameters",
                    "transform-es2015-shorthand-properties",
                    "transform-es2015-spread",
                    "transform-es2015-sticky-regex",
                    "transform-es2015-template-literals",
                    "transform-es2015-typeof-symbol",
                    "transform-es2015-unicode-regex",
                    "transform-regenerator",
                ],
                ast: false,
                filename: 'kata'
            }).code;
        } else {
            return require('babel-core').transform(code, {
                presets: ["stage-1", "node5", "react"],
                plugins: [
                    "angular2-annotations",
                    "transform-decorators-legacy",
                    "transform-class-properties",
                    "transform-flow-strip-types",
                ],
                ast: false,
                filename: 'kata'
            }).code;
        }
    }
    catch(ex) {
        var msg = ex.message;
        if (ex.loc) {
            // replace the line number since it is not what the user sees
            msg = msg.replace(/ \(\d*:\d*\)/, ":" + ex.loc.column)
            var lines = code.split('\n');
            msg += "\n" + lines[ex.loc.line - 1];
            msg += "\n";
            for(var i = 1;i < ex.loc.column; i++) {
                msg += ' ';
            }
            msg += '^';
        }
        throw new shovel.CompileError(msg);
    }
}
