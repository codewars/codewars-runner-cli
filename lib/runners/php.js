var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp'),
    config = require('../config');

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
        testIntegration: function (run)
        {
            switch (opts.testFramework)
            {
                case 'cw-2':
                    return prepareCw2(opts, run);
                case 'phpunit':
                    return preparePHPUnit(opts, run);
                default:
                    throw 'Test framework is not supported'
            }
        },
        sanitizeStdOut : function (output) {
           var lines = output.split('\n');
           return filterLines(lines).join('\n');
        },
        sanitizeStdErr : function (error) {
            error = error ? `\n${error}` : '';
            return error.replace(/(Uncaught Exception: Failed Test).*/g, '$1')
                        .replace(/\/runner\/.*\.php/g, 'input');
        }
    });
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
    temp.track();
    var dir = temp.mkdirSync('php');

    var code = `
        <?php
            use phpunit\\framework\\TestCase;
            ${opts.setup || ''}
            ${opts.solution}
            ${opts.fixture}
        ?>
    `;

    var file = util.codeWriteSync('php', code, dir, 'run.php');

    run({name: 'phpunit', 'args': ['--configuration=frameworks/php/phpunit/phpunit.xml', file]});
}

var filterLines = (function () {
    var blacklist = [
        /^\s*$/,
        /PHPUnit \d\.\d\.\d by Seb/,
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
    return function (lines) {
        return lines
            .filter(function (line) {
                return blacklist.every(function (regex) {
                    return !regex.test(line);
                });
            })
            .map(function(line) {
                // clean up filenames
                if(/\/tmp.*\.php/.test(line)) {
                    line = line.replace(/\/tmp\/[^\/]+\//, '');
                }
                return line;
            });
    };
}());