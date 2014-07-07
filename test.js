var opts = require("nomnom")
    .options({
        language: {
            abbr: 'l',
            help: 'The language to execute the code in'
        },
        // Not used...
        languageVersion: {
            abbr: 'V',
            full: 'language-version',
            help: 'The version of the language that you wish to use'
        },
        timeout: {
            help: 'The timeout to be used for running the code. If not specified a language specific default will be used',
            abbr: 't',
            default: '5000'
        },
        format: {
            help: 'The output format that will be returned. Options are "default" and "json"',
            default: 'default',
            choices: ['default', 'json'],
            abbr: 'fmt'
        }
    })
    .help('This utility will run unit tests for a specified language, or all of the tests.')
    .parse(),
    spec = opts.language ? ['test/runners/', opts.language, '_spec.js'].join('') : 'test/runners/*_spec.js',
    proc = require('child_process').spawn('mocha',[spec, '-t', opts.timeout]);

proc.stdout.on('data', function (data) {
    console.log(data.toString());
});

proc.on('close', function (code) {
    console.log('process exit code ' + code);
});