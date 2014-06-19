var opts = require("nomnom")
    .options({
        solution: {
          abbr: 'c',
          help: "solution code to run"
        },
        solutionFile: {
            abbr: 'C',
            full: 'solution-file',
            help: "solution file to run"
        },
        fixture: {
            abbr: 'f',
            help: 'Test fixture code to test with'
        },
        fixtureFile: {
            abbr: 'F',
            full: 'fixture-file',
            help: 'Test fixture code to test with'
        },
        setup: {
            abbr: 's',
            help: 'Setup code to be used for executing the code'
        },
        setupFile: {
            abbr: 'S',
            full: 'setup-file',
            help: 'Setup file to be used for executing the code'
        },
        language: {
            abbr: 'l',
            help: 'The language to execute the code in'
        },
        languageVersion: {
            abbr: 'V',
            full: 'language-version',
            help: 'The version of the language that you wish to use'
        },
        testFramework: {
            abbr: 't',
            full: 'test-framework',
            help: 'The language specific framework to run in'
        },
        timeout: {
            help: 'The timeout to be used for running the code. If not specified a language specific default will be used'
        },
        format: {
            help: 'The output format that will be returned. Options are "default" and "json"',
            default: 'default',
            choices: ['default', 'json'],
            abbr: 'fmt'
        },
        debug: {
            abbr: 'd',
            help: 'Print debugging info',
            flag: true
        },
        version: {
            abbr: 'v',
            flag: true,
            help: 'Print version and exit',
            callback: function () {
                return 'version 0.1.0'
            }
        }
    })
    .help('This utility will run code in a specified language, using the specified testing suite.')
    .parse();

require('./lib/opts').process(opts, function(opts) {
    if(opts) require('./lib/runners/' + opts.language + '.js').run(opts);
});




