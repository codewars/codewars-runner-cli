var run = require('./lib/runner').run,
    opts = require("nomnom")
      .options({
        code: {
          abbr: 'c',
          help: 'code to run'
        },
        fixture: {
          abbr: 'f',
          help: 'Test fixture code to test with'
        },
        setup: {
          abbr: 's',
          help: 'Setup code to be used for executing the code'
        },
        language: {
          abbr: 'l',
          help: 'The language to execute the code in'
        },
        languageVersion: {
          abbr: 'lv',
          help: 'The language version that should be used'
        },
        testFramework: {
          abbr: 't',
          full: 'test-framework',
          help: 'The language specific framework to run in'
        },
        shell: {
          abbr: 'sh',
          help: 'An optional shell script which will be ran within the sandbox environment before the code is executed'
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
        services: {
          abbr: 'srv',
          help: 'Enable a set of services, such as redis or mongodb. Multiple options can be specified',
          list: true,
          choices: ['mongodb', 'redis']
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
          callback: function() {
            return require('./lib/config').version;
          }
        }
      })
      .help('This utility will run code in a specified language, using the specified testing suite.')
      .parse();


run(opts);
