var shovel = require('../shovel'),
    config = require('../config');

module.exports.run = function run(opts, cb) {
    shovel.start(opts, cb, {
        solutionOnly: function (run) {
            var code = [opts.setup, opts.solution].join("\n");
            run({name: 'python', 'args': ['-c', code]});
        },
        fullProject: function (run) {
            var code;
            with (config.snippets.python) {
                switch (opts.testFramework) {
                    case 'cw-2':
                        code = [
                            addPythonFramework,
                            requireCw2,
                            opts.setup, opts.solution, opts.fixture].join('\n');
                        break;
                    case 'unittest':
                        code = [
                            opts.setup, opts.solution,
                            addPythonFramework,
                            requireUnittest,
                            opts.fixture,
                            runUnittest].join('\n');
                        break;
                    default:
                        throw new Error('test framework is not supported');
                }
            }
            run({name: 'python', 'args': ['-c', code]});
        },
        sanitizeStdErr: function (err) {
            // get rid of some of the noisy content. We remove line numbers since
            // they don't match up to what the user sees and will only confuse them.
            return err.replace(/File "(<string>)?", line \d*,/g, '')
                .replace(' (most recent call last)', '')
                .replace(' in in ', ' in ');
        }
    });
};
