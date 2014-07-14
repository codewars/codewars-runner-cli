var shovel = require('../shovel'),
    config = require('../config'),
    codeCompileSync = require('../util').codeCompileSync,
    fs = require('fs'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
    temp.track();
    var erlangCodeDir = temp.mkdirSync('erlang');
    shovel.start(opts, cb, {
        solutionOnly: function () {
            if (opts.setup) codeCompileSync('erlang', opts.setup, erlangCodeDir);
            return {
                name: 'erl',
                args: ['-pz', erlangCodeDir, '-noshell', '-eval', opts.solution],
                options: { env: {
                    HOME: process.env['HOME'],
                    ERL_CRASH_DUMP: "/dev/null"
                }}
            };
        },
        fullProject: function () {
            if (opts.setup) codeCompileSync('erlang', opts.setup, erlangCodeDir);
            codeCompileSync('erlang', opts.solution, erlangCodeDir);
            return {
                name: 'erl',
                args: ['-pz', erlangCodeDir, '-noshell', '-eval', opts.fixture],
                options: { env: {
                    HOME: process.env['HOME'],
                    ERL_CRASH_DUMP: "/dev/null"
                }}
            };
        }
    });
};