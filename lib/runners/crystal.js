var shovel = require('../shovel'),
    util = require('../util'),
    Convert = require('ansi-to-html'),
    convert = new Convert();

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      runCode({name: 'crystal', args: ['eval', opts.solution]});
    },
    testIntegration: function(runCode) {
      // instead of using a tmp directory we use the special crystal directory so that we can
      // use the installed shards without having to copy
      var dir = '/home/codewarrior/crystal';

      util.codeWriteSync('crystal', opts.solution, dir, 'solution.cr', true);
      util.codeWriteSync('crystal', opts.setup || '# no setup provided', dir, 'setup.cr', true);
      util.codeWriteSync('crystal', opts.fixture, dir, 'fixture.cr', true);

      var spec = `require "./formatter"`;
      if (opts.setup) spec += `\nrequire "./setup"`;
      spec += `\nrequire "./solution"`;
      spec += `\nrequire "./fixture"`;

      util.codeWriteSync('crystal', spec, dir, 'spec.cr', true);
      runCode({name: 'crystal', args: ['spec', 'spec.cr'], options: {cwd: dir}});
    },
    transformBuffer: function(buffer) {
      buffer.stdout = convert.toHtml(buffer.stdout);

      var finished = buffer.stdout.search(/(?! )\d* examples?/);
      if (finished > 0) {
        var orig = buffer.stdout;
        buffer.stdout = orig.substr(0, finished).replace("Failures:\n", "Failure Summary:\n");
      }

      // crystal likes to write its compile errors to stdout, so lets swap them around
      if (buffer.stdout.indexOf('Error in ') === 0) {
        buffer.stderr = buffer.stdout;
        buffer.stdout = '';
      }
    },
    outputType: 'raw'
  });
};
