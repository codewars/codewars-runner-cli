var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config'),
    temp = require('temp'),
    ruby = require('./ruby');

module.exports.run = function run(opts, cb)
{
    temp.track();
    var dir = temp.mkdirSync('bash');

    shovel.start(opts, cb, {
        solutionOnly: function (exec)
        {
            var file = util.codeWriteSync('bash', opts.solution, dir, 'solution.sh')
            exec({name: opts.languageVersion || 'bash', 'args': [file]});
        },
        testIntegration: function (exec)
        {
            prepareSetup(opts);
            opts.solution = "#"
            opts.fixture = `\`rm -rf /workspace/fixture.rb\` ; ${opts.fixture}`
            ruby.prepareRSpec(opts, exec);
        }
    });
};

function prepareSetup(opts) {
    opts.setup = `
        $shell = "${opts.languageVersion || 'bash'}"  
        require '/runner/frameworks/ruby/shell'
        ${opts.setup || ''}
    `;
}