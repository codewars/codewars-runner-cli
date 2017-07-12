var shovel = require('../shovel'),
    ruby = require('./ruby');

module.exports.run = function run(opts, cb) {

  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      prepareSetup(opts);
      var code = `${opts.setup}\nputs run_sql`;
      runCode({name: 'ruby', 'args': ['-e', code]});
    },
    testIntegration: function(runCode) {
      prepareSetup(opts);
      opts.solution = "#";
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
      return err.split("\n").filter(l => l.indexOf("from /usr/local") == -1).join("\n");
    }
  });
};

function prepareSetup(opts) {
  // this will set environment variables
  connectDb(opts);

  if (!opts.projectMode) {
    opts.setup = [
      "require '/runner/frameworks/ruby/sql'",
      opts.setup || ''
    ].join("\n");
  }
}

function connectDb(opts) {
  opts.env = opts.env || {};
  var type = opts.languageVersion || 'sqlite',
      database = ":memory:";

  if (type == 'postgres') {
    database = dbName(opts) || 'postgres';
  }

  // these variables will be passed on to the ruby process
  process.env.DATABASE_TYPE = type;
  process.env.DATABASE_NAME = database;
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
    opts.services.push(name);
  }
}
