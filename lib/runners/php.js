var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config');

var PASSED = '<PASSED::>';
var FAILED = '<FAILED::>';
var NEWLINE = '<:LF:>';
// var NEWLINE = '\n';

module.exports.run = function run(opts, cb)
{
    shovel.start(opts, cb, {
        solutionOnly: function (run)
        {
            var code = opts.solution;

            if (opts.setup)
            {
                code = opts.setup + ';\n' + code;
            }

            run({name: 'php', 'args': ['-r', code]});
        },
        fullProject: function (run)
        {
            var code = [
                config.snippets.php.bootstrap,
                (opts.setup || ''),
                config.snippets.php.beforeUserScript,
                opts.solution,
                config.snippets.php.afterUserScript,
                config.snippets.php.classBegin,
                opts.fixture,
                config.snippets.php.classEnd,
                config.snippets.php.execute
            ].join('\n');

            run({name: 'php', 'args': ['-r', code]});
        },
        //sanitizeStdOut : function (output) {
        //    var lines = output.split('\n');
        //    var success = (lines[lines.length - 2] || '').slice(0, 4) === 'OK (';
        //
        //    if (success) {
        //        return PASSED + ' All tests passed.';
        //    }
        //
        //    return FAILED + filterLines(lines).join(NEWLINE);
        //}
    });
};

var filterLines = (function () {
    var blacklist = [
        /^PHPUnit \d\.\d\.\d by Seb/,
        /^\d\) KataTest::testSolution/,
        /^Time: \d+ ms, Memory:/,
        /^There was \d+ failure/,
        /^FAILURES!$/,
        /^Tests: \d+, Assertions:/

    ];

    /**
     * Strip lines from output we don't want
     *
     * @param array lines
     * @return array
     */
    return function (lines) {
        return lines.filter(function (line) {
            return blacklist.every(function (regex) {
                return !regex.test(line);
            });
        });
    };
}());