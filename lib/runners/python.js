var shovel = require('../shovel'),
    config = require('../config');

module.exports.run = function run(opts, cb)
{
    shovel.start(opts, cb, {
        solutionOnly: function ()
        {
            var code = [opts.setup, opts.solution].join("\n");
            return {name: 'python', 'args': ['-c', code]};
        },
        fullProject: function ()
        {
            var code = [config.snippets.python.requireCw2, opts.setup, opts.solution, opts.fixture].join("\n");
            return {name: 'python', 'args': ['-c', code]};
        },
        sanitizeStdErr: function(err)
        {
            // get rid of some of the noisy content. We remove line numbers since
            // they don't match up to what the user sees and will only confuse them.
            return err.replace(/File "(<string>)?", line \d*,/g, '')
                      .replace(' (most recent call last)', '')
                      .replace(' in in ', ' in ');
        }
    });
};