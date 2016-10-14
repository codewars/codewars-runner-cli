var shovel = require('../shovel'),
    config = require('../config'),
    ruby = require('./ruby');

module.exports.run = function run(opts, cb) {

    shovel.start(opts, cb, {
        solutionOnly: function (exec) {
            prepareSetup(opts);
            var code = `${opts.setup}\nputs run_sql`;
            exec({name: 'ruby', 'args': ['-e', code]});
        },
        testIntegration: function (exec) {
            prepareSetup(opts);
            opts.solution = "#"
            if (opts.testFramework === 'cw-2') {
                ruby.prepareCw2(opts, exec);
            }
            else {
                ruby.prepareRSpec(opts, exec);
            }
        },
        modifyOpts: function(opts) {
            switch (opts.languageVersion) {
                case 'postgres':
                    addService(opts, 'postgres');
                    break;
            }
        },
        sanitizeStdErr: function(err) {
            return err.split("\n").filter(l => l.indexOf("from /usr/local") == -1).join("\n")
        }
    });
};

function prepareSetup(opts) {
    opts.setup = `
        require '/runner/frameworks/ruby/sql'
        DB = Sequel.${connectDb(opts.languageVersion)}
        ${opts.setup || ''}
    `
}


function connectDb(type) {
    type = type || 'sqlite';
    if (type == 'postgres') {
        return `connect("postgres://localhost/postgres")`;
    }

    return type;
}

function addService(opts, name) {
    opts.services = opts.services || [];
    if (opts.services.indexOf(name) === -1) {
        opts.services.push(name)
    }
}
