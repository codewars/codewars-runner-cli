var opts = require("nomnom")
    .option('solution', {
      abbr: 'c',
      help: "solution code to run"
    })
    .option('solutionFile', {
      abbr: 'cf',
      full: 'solution-file',
      help: "solution file to run"
    })
    .option('fixture', {
        abbr: 'f',
        help: 'Test fixture code to test with'
    })
    .option('fixtureFile', {
        abbr: 'ff',
        full: 'fixture-file',
        help: 'Test fixture code to test with'
    })
    .option('setup', {
        abbr: 's',
        help: 'Setup code to be used for executing the code'
    })
    .option('setupFile', {
        abbr: 'sf',
        full: 'setup-file',
        help: 'Setup file to be used for executing the code'
    })
    .option('language', {
        abbr: 'l',
        help: 'The language to execute the code in'
    })
    .option('languageVersion', {
        abbr: 'lv',
        full: 'language-version',
        help: 'The version of the language that you wish to use'
    })
    .option('testFramework', {
        abbr: 't',
        full: 'test-framework',
        help: 'The language specific framework to run in'
    })
    .option('timeout', {
        help: 'The timeout to be used for running the code. If not specified a language specific default will be used'
    })
    .option('debug', {
        abbr: 'd',
        help: 'Print debugging info',
        flag: true
    })
    .option('version', {
        abbr: 'v',
        flag: true,
        help: 'Print version and exit',
        callback: function(){ return 'version 0.1.0' }
    })
    .help('This utility will run code in a specified language, using the specified testing suite.')
    .parse();

require('./lib/opts').process(opts, function(opts) {
    if(opts) require('./lib/runners/' + opts.language + '.js').run(opts)
});




