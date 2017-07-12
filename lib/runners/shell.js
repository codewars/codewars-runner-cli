var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp'),
    ruby = require('./ruby');

module.exports.run = function run(opts, cb) {
  temp.track();
  var dir = temp.mkdirSync('bash');

  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      var file = util.codeWriteSync('bash', opts.solution, dir, 'solution.sh');
      runCode({name: opts.languageVersion || 'sh', 'args': [file]});
    },
    testIntegration: function(runCode) {
      process.env.SHELL = opts.languageVersion || 'sh';

      if (!opts.projectMode) {
        prepareSetup(opts);
        opts.solution = "#";
        opts.fixture = `\`rm -rf /workspace/fixture.rb\` ; ${opts.fixture}`;
      }

      ruby.prepareRSpec(opts, runCode);
    }
  });
};

function prepareSetup(opts) {
  opts.setup = `
        require '/runner/frameworks/ruby/shell'
        ${opts.setup || ''}
    `;
}
