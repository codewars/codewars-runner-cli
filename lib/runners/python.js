var shovel = require('../shovel'),
    config = require('../config');

module.exports.run = function run(opts, cb) {
    shovel.start(opts, cb, {
        solutionOnly: function (run) {
            var code = [opts.setup, opts.solution].join("\n");
            runVersion(opts.languageVersion, code, run);
        },
        testIntegration: function (run) {
            var code;
            var snippets = config.snippets.python;
            switch (opts.testFramework) {
                case 'cw':
                case 'cw-2':
                    code = [
                        snippets.addPythonFramework,
                        snippets.requireCw2,
                        opts.setup,
                        opts.solution,
                        opts.fixture
                    ].join('\n');
                    break;
                case 'unittest':
                    code = [
                        opts.setup,
                        opts.solution,
                        snippets.addPythonFramework,
                        snippets.requireUnittest,
                        opts.fixture,
                        snippets.runUnittest
                    ].join('\n');
                    break;
                default:
                    throw new Error('test framework is not supported');
            }
            runVersion(opts.languageVersion, code, run);
        },
        sanitizeStdErr: function (err) {
            // get rid of some of the noisy content. We remove line numbers since
            // they don't match up to what the user sees and will only confuse them.
            return err
                .replace(/>\n\n+</g, '>\n<')
                .replace(/File "(<string>)?", line \d*,/g, '')
                .replace(' (most recent call last)', '')
                .replace(' in in ', ' in ');
        }
    });
};

function runVersion(version, code, run) {
    var name = "python";
    if (version && version[0] == '3') name = "python3";

    run({name: name, 'args': ['-c', code]});
}
