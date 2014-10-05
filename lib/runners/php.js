var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config');

module.exports.run = function run(opts, cb)
{
    shovel.start(opts, cb, {
        solutionOnly: function ()
        {
            var code = opts.solution;

            if (opts.setup)
            {
                code = opts.setup + ';\n' + code;
            }

            return {name: 'php', 'args': ['-r', code]};
        },
        fullProject: function ()
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

            return {name: 'php', 'args': ['-r', code]};
        },
        sanitizeStdOut : function (output) {
            var lines = output.split('\n');
            var success = (lines[lines.length - 2] || '').slice(0, 2) === 'OK';

            if (success) {
                return 'All tests passed';
            }

            return lines.join('\n');
        }
    });
};
