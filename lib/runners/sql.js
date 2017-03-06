var shovel = require('../shovel'),
    config = require('../config'),
    ruby = require('./ruby');

module.exports.run = function run(opts, cb) {

    shovel.start(opts, cb, {
        solutionOnly: function (runCode) {
            prepareSetup(opts);
            var code = `${opts.setup}\nputs run_sql`;
            runCode({name: 'ruby', 'args': ['-e', code]});
        },
        testIntegration: function (runCode) {
            prepareSetup(opts);
            opts.solution = "#"
            if (opts.testFramework === 'cw-2') {
                ruby.prepareCw2(opts, runCode);
            }
            else {
                ruby.prepareRSpec(opts, runCode);
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
        ${connectDb(opts)}
        require '/runner/frameworks/ruby/sql'
        ${opts.setup || ''}
    `
}

function connectDb(opts) {
    var type = opts.languageVersion || 'sqlite',
        database = ":memory:",
        code = `DB = Sequel.${type}`;

    if (type == 'postgres') {
        database = dbName(opts) || 'postgres';
        code = `DB = Sequel.connect(\"postgres://localhost/${database}\")`;
    }

    return `CONNECT_SQL = '${code}';DATABASE = "${database}";`;
}

// checks the first two lines of the setup code to see if a database name was specified. This is done
// because the first line can be used as a comment explaining why its there
function dbName(opts) {
    // both @use-database and @config: use-database are supported for backwards compatibility
    var match = (opts.setup || opts.fixture || '').match(/^# ?@(?:config: )?use-database (\w*)/m);
    return match ? match[1] : opts.useDatabase || null;
}

function addService(opts, name) {
    opts.services = opts.services || [];
    if (opts.services.indexOf(name) === -1) {
        opts.services.push(name)
    }
}
