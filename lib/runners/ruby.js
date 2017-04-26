var shovel = require('../shovel'),
    util = require('../util');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    modifyOpts: function() {
      // if a github repo was provided, add the workspace to the load path so that requires work correctly
      if (opts.githubRepo || opts.files || opts.gist) {
        opts.setup = `$LOAD_PATH << '/home/codewarrior'\n${opts.setup || ''}`;
      }
    },
    solutionOnly: function(runCode) {
      var code = opts.solution;

      if (opts.setup) {
        code = opts.setup + '\n' + code;
      }

      runCode({name: 'ruby', args: ['-e', code], options: {cwd: opts.dir}});
    },
    testIntegration: function(runCode) {
      switch (opts.testFramework) {
        case 'cw':
        case 'cw-2':
          return prepareCw2(opts, runCode);
        case 'rspec':
          return prepareRSpec(opts, runCode);

        default:
          throw 'Test framework is not supported';
      }
    },
    sanitizeStdErr: function(error) {
      return error.replace(/[\w/-]*(cw-2.rb):[\d]*:in( `(measure|wrap_error|it|describe)'<:LF:>)?/g, '').replace(/-e:[\d]*:in/g, '').replace('  ', ' ').replace(/<:LF:> `(block in )?(<main>|describe|it)'/g, '').replace('  ', ' ');
    },
    sanitizeStdOut: function(stdout) {
      return this.sanitizeStdErr(stdout);
    }
  });
};

function prepareCw2(opts, exec) {
  const requireFramework = "require('/runner/frameworks/ruby/cw-2')";

  // by default cw-2 concatenates files so this special option causes separate files to be used instead
  if (opts.entryPath || opts.useSeparateFiles) {
    exec({
      name: 'ruby',
      args: [prepareEntryFile(opts, requireFramework)],
      options: {cwd: opts.dir}
    });
  }
  else {
    var code = [requireFramework];

    if (opts.setup) {
      code.push(opts.setup);
    }

    code.push(opts.solution);
    code.push(opts.fixture);

    exec({
      name: 'ruby',
      args: ['-e', code.join('\n')],
      options: {cwd: opts.dir}
    });
  }
}

function prepareRSpec(opts, exec) {
  exec({
    name: 'rspec',
    args: [prepareEntryFile(opts), '--require', '/runner/frameworks/ruby/cwrspecformatter.rb', '--format', 'CwRSpecFormatter'],
    options: {cwd: opts.dir}
  });
}

// used when a single file will be used as the entry point. It will include the other files separately
function prepareEntryFile(opts, require) {
  // if there is no require and an entryPath is provided than just use that file directly
  if (!require && opts.entryPath) return opts.entryPath;

  var entry = [
    "`rm -rf /workspace/entry.rb`",
    require || ''
  ];

  if (opts.entryPath) {
    entry.push(`require "${opts.entryPath}"`);
  }
  else {
    if (opts.setup) {
      entry.push(`require "${util.codeWriteSync('ruby', opts.setup, opts.dir, 'setup.rb')}"`);
      // have the file remove itself from the file system after it is loaded, so that it cannot be read by users trying to solve
      entry.push("`rm -rf /workspace/setup.rb`");
    }

    entry.push(`require "${util.codeWriteSync('ruby', opts.solution, opts.dir, 'solution.rb')}"`);

    if (opts.fixture) {
      entry.push(`require "${util.codeWriteSync('ruby', opts.fixture, opts.dir, '.spec.rb')}"`);
    }
  }

  return util.codeWriteSync('ruby', entry.join('\n'), opts.dir, '.entry.rb');
}

module.exports.prepareRSpec = prepareRSpec;
module.exports.prepareCw2 = prepareCw2;
